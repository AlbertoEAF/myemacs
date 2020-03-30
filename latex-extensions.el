
(defun latex-select-frac()
  "Selects the whole fraction in latex."
  (interactive)
  (set-mark (point))
  (forward-list 2)
  (set-mark (pos))
  (backward-list 1))


(defun latex-add-begin-end(name)
  "Adds a \begin and \end with the content you specify."
  (interactive "sName:")
  (setq line (format "\\begin{%s}\n\n\\end{%s}\n" name name))
  (insert line)
  (previous-line)
  (previous-line))

