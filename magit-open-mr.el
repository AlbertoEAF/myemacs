(require 'seq)
(require 'cl-lib)

(defun magit-open-mr ()
  "Use this method to open an MR directly after pushing a new branch through magit (requires that only one magit process or repo is open)."
  (interactive)
  (with-current-buffer (get-single-magit-process-buffer-name)
    (let* ((lines (buffer-lines-to-list))
           (mr-header-index-from-bottom (cl-position-if (lambda (line)
                                                          (string-match ".*To create a merge request for af-broken-dc-link-stag, visit:.*$" line))
                                                        (reverse lines))))
      (when (null mr-header-index-from-bottom)
        (error "Couldn't find a link to an MR to open!"))
      (let* ((url-line (nth (- (length lines) mr-header-index-from-bottom) lines))
             (mr-url (if (string-match "remote:.*\\(https://[^ ]*\\)" url-line)
                         (match-string 1 url-line)
                       (error "Couldn't extract MR URL from line %s" url-line))))
        (message "Opening link=%s to create an MR." mr-url)
        (browse-url mr-url)))))

(defun drop-until (pred lst)
  "Drop elements from LST until PRED is false for the first time."
  (seq-drop-while pred lst))

(defun buffer-lines-to-list ()
  "Return a list of all lines in the current buffer as strings."
  (split-string (buffer-string) "\n" t))


(defun list-buffer-names()
  "Returns a list with the names of all buffers."
  (interactive)
  (let ((buffer-names (mapcar #'buffer-name (buffer-list))))
    (message "Open buffers %s" buffer-names)
    buffer-names))

(defun get-magit-process-buffer-names()
  "Returns the list of buffer names that are from magit-process."
  (cl-remove-if-not (lambda (buf-name)
                      (string-match "^magit-process: .*$" buf-name))
                    (list-buffer-names)))

(defun get-single-magit-process-buffer-name()
  "Ensures there is exactly one magit-process buffer and returns it."
  (let ((magit-ps-buffer-names (get-magit-process-buffer-names)))
    (unless (= 1 (length magit-ps-buffer-names))
      (error "Expected to find a single magit-process buffer, but found several! %s" magit-ps-buffer-names))
    (car magit-ps-buffer-names)))
