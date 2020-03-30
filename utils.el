; Ferramentas para laboratorios


(defun scientific-trim_exp ()
  (interactive)
  (replace-regexp "{\\(-\\|\\)0+\\([0-9]+\\)}" "{\\1\\2}"))


(defun excel2scientific ()
  (interactive)
  (replace-regexp "\\([-\\.0-9]+\\)E\\([-+0-9]+\\)" "\\1\\\\times10^{\\2}"))

(defun scientific-mathify ()
  (interactive)
  (replace-regexp "\\([-\\.0-9]+\\).times10^{\\([-+0-9]+\\)}" "$\\1\\\\times10^{\\2}$"))

;; "numero cientifico \pm erro decimal"
;; \([-\.0-9]+\)E\([-+0-9]+\)\\pm\([.0-9]+\) -> (\1\\pm\3)\\times10^{\2}

(defun red ()
  (interactive)
  (insert "{\\color{red}  }")
  (backward-char 1))


;;; PDFLATEX commands -> split this to it's own file please

(defvar *pdflatex-main-tex-buffer-file-name* nil)
(defvar *pdflatex-main-tex-buffer-name* nil)
(defvar *pdflatex-pdf-reader* "evince")

(defun get-aux (tex-file)
  "Returns the .aux name counterpart of the .tex filepath."
  (concat (substring tex-file 0 -4) ".aux"))

(defun pdflatex ()
  (interactive)
  (save-some-buffers t)

  (if (not *pdflatex-main-tex-buffer-file-name*)
      (set-main-tex))
  (setq tex-file *pdflatex-main-tex-buffer-file-name*)
;  (if (not tex-file)
;      (setq tex-file (buffer-file-name)))

  (let ((dir default-directory)
    (main-dir (file-name-directory tex-file)))
    (cd main-dir)
    (message "Compiling %s" tex-file)
    (message
     (shell-command-to-string (concat "pdflatex \"" tex-file "\"")))
    (cd dir)))


(defun pdflatex+bib ()
  "Compiles with bibliography update as well."
  (interactive)
  (save-some-buffers t)
  (setq tex-file *pdflatex-main-tex-buffer-file-name*)
  (if (not tex-file)
      (setq tex-file (buffer-file-name)))
  (let ((dir default-directory)
    (main-dir (file-name-directory tex-file)))
    (cd main-dir)
    (message "Compiling %s and bibliography" tex-file)
    (message
     (shell-command-to-string (concat "pdflatex \"" tex-file "\""))
     (shell-command-to-string (concat "bibtex \"" (get-aux tex-file) "\""))
     (shell-command-to-string (concat "pdflatex \"" tex-file "\""))
     (shell-command-to-string (concat "pdflatex \"" tex-file "\""))
     )
    (cd dir)))

(defun set-main-tex()
  "Sets the main latex file for latex to compile"
  (interactive)
  (setq *pdflatex-main-tex-buffer-file-name* (buffer-file-name))
  (setq *pdflatex-main-tex-buffer-name* (buffer-name))
  (message "Main tex file set to %s" (buffer-name)))

(defun jump-to-main-tex()
  "Jumps to the buffer set by (set-main-tex)."
  (interactive)
  (switch-to-buffer *pdflatex-main-tex-buffer-name*)
  (message "Jumped to main tex file %s" (buffer-name)))




(defun first-t(list)
  "Returns the first non-nil element from the list. If none exists returns nil."
  (let ((e (car list)))
    (if e
    e
      (first-t (cdr list)))))

(defun replace-extension(tex-name new-extension)
  "Replaces the old extension in favour of the new one."
  (concat (substring tex-name 0 -3) new-extension))



(defun pdflatex-open-pdf()
  "Opens the pdf reader for the main tex file."
  (interactive)
  (let ((tex-file (first-t (list *pdflatex-main-tex-buffer-file-name* (buffer-file-name)))))
    (save-window-excursion
      (shell-command (concat *pdflatex-pdf-reader* " " (replace-extension tex-file "pdf") " &")))))







;;; Aliases for fast latex typing

(defun latex-power (arg)
  "Writes a latex power asking the user for what to put inside first."
  (interactive "sExp:")
  (insert (concat "^{" arg "}")))


(defun latex-power-parenthesis (arg)
  "Writes a latex power asking the user for what to put inside first."
  (interactive "sExp:")
  (insert (concat "^{(" arg ")}")))


(defun latex-warning ()
  (interactive)
  (insert "{\\color{red} WARN }"))
