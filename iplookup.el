;; iplookup.el
;;
;; Author: Bill St. Clair, bill@billstclair.com
;;
;; Support for looking up host names in a web server log file
;; Two interactive commands are of interest here:
;;
;;   iplookup interprets the buffer from point to space or end-of-line
;;   as an IP address and does a reverse DNS lookup using the default
;;   name server. If it finds a name, it replaces the IP in the buffer.
;;
;;   iplookup-rest looks up IP addresses starting at point and at
;;   the beginning of each line in the rest of the buffer.
;;   This allows you to convert IP addresses to host name in
;;   a web server log file.

(provide 'iplookup)

;; Use the Common Lisp library (for make-hash-table)
(require 'cl)

(defun get-host (host)
  "Lookup a host name from the default name server.
Return a two-element list: (name addresses)"
  (let* ((old-b (current-buffer))
         (b (get-buffer "*nslookup*"))
         (p (get-process "nslookup")))
    (when (or (null b) (null p))
      (nslookup)
      (setq b (current-buffer))
      (setq p (get-process "nslookup")))
    (unwind-protect
        (progn
          (set-buffer b)
          (goto-char 0)
          (delete-char (1- (point-max)))
          (insert host)
          (comint-send-input)
          (loop
           (goto-char 0)
           (when (search-forward "\n> " nil t)
             (return))
           (accept-process-output nil 0 100))
          (goto-char 0)
          (let* ((pos (search-forward "Name:    " nil t)))
            (if (null pos)
                nil
              (let* ((epos (line-end-position))
                     (name (buffer-substring pos epos)))
                (search-forward "Address")
                (setq pos (search-forward ":  ")
                      epos (line-end-position))
                (list name (buffer-substring pos epos))))))
      (set-buffer old-b))))

(defvar *iplookup-hash* (make-hash-table :test 'equal))
(defvar *iplookup-autosave-file* nil)
(defvar *iplookup-autosave-count* 0)

(defun get-host-cached (host)
  "Same as get-host, but looks in a cache before going to the name server."
  (let ((name (gethash host *iplookup-hash*)))
    (unless name
      (setq name (car (get-host host)))
      (cl-puthash host (or name 'nonexistant) *iplookup-hash*))
    (if (eq name 'nonexistant)
        nil
      name)))

(defun iplookup ()
  "Replace the IP address at point with its host name."
  (interactive)
  (save-window-excursion
    (save-excursion
      (iplookup-internal))))

(defun iplookup-internal ()
  "Replace the IP address at point with its host name."
  (let* ((pos (point))
         (epos (1- (search-forward-regexp "[ \n]")))
         (host (buffer-substring pos epos))
         (name (get-host-cached host)))
    (when name
      (goto-char pos)
      (delete-char (length host))
      (insert name))))

;; Need to test this on a big log file. I think I fixed the crashing problem.
(defun iplookup-rest ()
  "Lookup the IP addresses of the beginnings of lines in the rest of the buffer"
  (interactive)
  (save-window-excursion
    (loop
     (if (eql (point) (point-max)) (return))
     (iplookup-internal)
     (goto-char (1+ (line-end-position))))))

(defun save-iplookup-table (&optional file)
  "Save the iplookup mapping table to the autosave file"
  (interactive)
  (when (null file) (setq file *iplookup-autosave-file*))
  (when (null file) (setq file (read-from-minibuffer "File name: ")))
  (let ((buf (generate-new-buffer file)))
    (maphash (function
              (lambda (key value)
                (unless (eq value 'nonexistant)
                  (prin1 key buf)
                  (princ " " buf)
                  (prin1 value buf)
                  (terpri buf))))
             *iplookup-hash*)
    (set-buffer buf)
    (set-visited-file-name file t)
    (save-buffer)
    (kill-buffer buf)
    (setq *iplookup-autosave-count* (hash-table-count *iplookup-hash*))))

(defun read-iplookup-table (file)
  (let ((buf (find-file-noselect file t))
        (hash (make-hash-table :test 'equal)))
    (ignore-errors
      (loop
       (let* ((name (read buf))
              (value (read buf)))
         (cl-puthash name value hash))))
    (kill-buffer buf)
    (setq *iplookup-hash* hash)))

(defun kill-emacs-save-iplookup-table ()
  (when (and *iplookup-autosave-file*
             (not (eql *iplookup-autosave-count*
                       (hash-table-count *iplookup-hash*))))
    (save-iplookup-table *iplookup-autosave-file*)))

(defun autosave-iplookup-table (file)
  (setq *iplookup-autosave-file* file)
  (when (eql 0 (hash-table-count *iplookup-hash*))
    (read-iplookup-table file)
    (setq *iplookup-autosave-count* (hash-table-count *iplookup-hash*)))
  (push '(lambda () (kill-emacs-save-iplookup-table)) kill-emacs-hook))
