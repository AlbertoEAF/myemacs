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
    (let* ((p1 (region-beginning)) (p2 (region-end))
           (a (min p1 p2)) (b (max p1 p2)))
      (goto-char b)
      (insert-char (smart-pair-closing-char chr))
      (goto-char a)
      (insert-char chr))))

(provide 'smart-pair)
