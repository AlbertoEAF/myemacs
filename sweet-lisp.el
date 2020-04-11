


(forward-line -2)
;;; sweet-lisp --- Improve support for sweet-expressions

;;; Commentary:

(thing-at-point 'whitespace)
;;; Code:

(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt
      txt))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun current-line-starts-with-space-p ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]].*")))

(defun line-at-sweet-block-p ()
  "T if inside a sweet block."
  (interactive)
  (save-excursion
    (let ((initial-line (line-number-at-pos))
          (searching t)
          (is-sweet nil))
      (while (equal searching t)
        (beginning-of-line)

        (let ((line-char0 (buffer-substring-no-properties (point) (1+ (point)))))
          (when (not (or (current-line-empty-p)
                         (current-line-starts-with-space-p)))
            (if (string= line-char0 "(") (setf is-sweet nil)
              (setf is-sweet t))
            (setf searching nil)))
          
        (when (minusp (forward-line -1))
          (setf searching nil)))
      (message "at-sweet-block? %s" is-sweet)
      is-sweet)))

(defun any-line-with-sweet-region-p ()
  ""
  (interactive)
  (message "TODO")
  t)

(defun my-lisp-pre-indent-command (&optional arg)
  "Hook for indentation in Lisp."
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      ;;; region
      (if (any-line-with-sweet-region-p) (message "Skipping region indent. Found a sweet block.")
        (indent-for-tab-command))
      
      ;;; no region - single line
      (if (line-at-sweet-block-p) (message "Skipping indent. At sweet block.")
        (indent-for-tab-command))))


(define-key lisp-mode-map (kbd "<tab>") 'my-lisp-pre-indent-command-line)

(define-key parinfer-region-mode-map (kbd "<tab>") 'my-lisp-pre-indent-command)


;; 'parinfer-shift-right


(provide 'sweet-lisp)
;;; sweet-lisp.el ends here
