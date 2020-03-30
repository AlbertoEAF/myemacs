
(defun my-rsync-from-dired-mark (target)
  "Rsyncs. SRC -> DEST."
  (interactive
   (list (read-file-name "rsync to:" (dired-dwim-target-directory))))
  
  (let* ((default-directory "~")
         (cmd (format "rsync -av --info=progress2  %s %s" (my-rsync-get-marked-dired-os-uris-string) target))
         (pretty-cmd (format "echo \"%s\" && %s" cmd cmd)))
    (message "Executing <%s> ..." cmd)
    (async-shell-command cmd) ; pretty-cmd
    ))

(defun my-rsync-get-marked-dired-os-uris-string ()
  (mapconcat 'my-rsync-os-uri (my-rsync-get-dired-mark-absolute-filepaths) " "))

(defun my-rsync-get-dired-mark-absolute-filepaths ()
  (split-string (dired-copy-filename-as-kill 0) " "))

(defun my-rsync-os-uri (str)
  (interactive "sString:")
  (string-remove-prefix "/ssh:" str))




