(define-minor-mode smart-pair
  "Smartly pair by certain characters as desired, and appropriately according to document type.
It is supposed to be a better alternative to autopair and the latest electric-pair-mode."
  :lighter " smart-pair"
  )

(defun smart-pair-closing-char (open-char)
  (cond ((eq open-char ?\() ?\))
        ((eq open-char ?\[) ?\])
        (t open-char)))

(defun smart-pair-region-with (chr)
  (interactive "cChar to encase region with:")
  (unless (region-active-p)
    (throw 'smart-pair "No region is active!"))
  (save-excursion
    (goto-char (region-end))
    (insert-char (smart-pair-closing-char chr))
    (goto-char (region-beginning))
    (insert-char chr)))

(provide 'smart-pair)
