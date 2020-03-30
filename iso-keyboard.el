(global-set-key (kbd "<dead-tilde> <dead-tilde>") 'insert-tilde)
(global-set-key (kbd "S-<dead-circumflex> S-<dead-circumflex>") 'insert-hat)

(defun insert-hat ()
  (interactive)
  (insert "^"))

(defun insert-tilde ()
  (interactive)
  (insert "~"))
