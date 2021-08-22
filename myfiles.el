;;; myfiles.el --- mode for recurrent files.


;;; Code:


(defvar *myfiles-list* nil "List of myfiles files")

(defvar myfiles-mode-hook nil)


(defvar myfiles-mode-map
  (let ((map (make-keymap))) ;; make-sparse-keymap?
    (define-key map (kbd "C-c f") 'myfiles-go-to-nth)
    (define-key map (kbd "g") 'myfiles-prefix-go-to-nth)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "f") 'myfiles-jump-to-file-at-cursor)
    map)
  "Keymap for myfiles-mode major mode.")

(defun myfiles-write-buffer-options ()
  "Write the options to the buffer."
  (let ((i 0))
    (while (< i (length *myfiles-list*))
      (insert (format "   %s )  %s\n" i (elt *myfiles-list* i)))
      (setq i (1+ i)))))

(defun myfiles-go-to-nth (idx)
  "Jump to the files ith-position IDX"
  (interactive "nGo to file #?")
  (find-file (elt *myfiles-list* idx)))

(defun myfiles-prefix-go-to-nth (idx)
  "Jump to the files at index IDX which is received by a prefix."
  (interactive "NGo to file #?")
  (find-file (elt *myfiles-list* idx)))

(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun myfiles-get_buffer_line_tag ()
  "Gets the jump argument from the files buffer"
  (interactive)
  (string-trim (car (split-string (thing-at-point 'line) ")"))))


(defun myfiles-jump-to-file-at-cursor ()
  "Jumps to the tag at the current line."
  (interactive)
  (myfiles-go-to-nth (string-to-number (myfiles-get_buffer_line_tag))))


(defun myfiles-buffer-init ()
  "Creates the myfiles buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Myfiles list:\n\n\n")
    (myfiles-write-buffer-options)))

(defun myfiles-load-myfiles-list ()
  "Loads the *myfiles-list* from the file."
  ; Dummy implementation: hardcoded
  ;(setq *myfiles-list* (list "asdf" "badsf"))
  ;(message "Loaded!"))
  (setq *myfiles-list* (read-lines (concat myemacs-dir "myfiles.mfl"))))



(define-derived-mode myfiles-mode special-mode "Myfiles"
  "Myfiles special mode to select your set of desired files faster."
  ;(use-local-map myfiles-mode-map)
  ;(run-hooks 'myfiles-mode-hook)
  ;(message "In myfiles mode!"))
  (myfiles-load-myfiles-list)
  )

(defun myfiles ()
  (interactive)
  (switch-to-buffer "myfiles")
  (myfiles-mode)
  (myfiles-buffer-init)
  )


(define-key global-map (kbd "C-c o") 'myfiles)


