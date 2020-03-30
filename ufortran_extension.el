;;
;;    UTransForm emacs extension
;;
;; Programmer: Alberto Ferreira
;; Release date: 11/10/2013
;;


(setq *ufortran_path* "~/UTransForm/ufortran2.py")

(defun trim-newline (str)
  "Recursively trims newline characters"
  (if (string-equal "\n" (substring str -1))
      (trim-newline (substring str 0 -1))
    str))

(defun get-start-delimiter (expression delimiters)
  "If the string starts with one of the delimiters returns that one."
  (dolist (delim delimiters)
    (if (and (> (length expression) (length delim))
         (string= delim (substring expression 0 (length delim))))
        (return delim))))

(defun get-end-delimiter (expression delimiters)
  "If the string ends with one of the delimiters returns that one."
  (dolist (delim delimiters)
    (let ((expr-len (length expression))  (delim-len (length delim)))
      (if (and (> expr-len delim-len)  (string= delim (substring expression (- expr-len delim-len))))
      (return delim)))))

(defun latex-get-start-delimiter (expression)
  " Removes math latex delimiter on the beginning "
  (get-start-delimiter expression '("\\be" "\\begin{equation}" "$$" "$")))

(defun latex-get-end-delimiter (expression)
  " Removes the trailing math latex delimiter "
  (get-end-delimiter expression '("\\ee" "\\end{equation}"  "$$" "$")))

(require 'cl)

(defun latex-strip-eq-delimiters (str)
  "Returns the list (delimiter-stripped-string start-delimiter end-delimiter)"
  (let ((start-delim (latex-get-start-delimiter str))
    (end-delim   (latex-get-end-delimiter   str)))
    (values (substring str (length start-delim) (- (length str) (length end-delim))) start-delim end-delim)))

(defun run-ufortran-on-line ()
  "Runs ufortran in --Latex+ mode on the formula on the current line and returns the result."
  (interactive)
  (multiple-value-bind (eq start-delim end-delim) (latex-strip-eq-delimiters (get-line))
    (let* ((cmd (concat "python " *ufortran_path* " -Latex " "\"" eq "\""))
       (ufortran_eq (trim-newline (shell-command-to-string cmd))))
      (message "CMD[%s]: %s ===> %s" cmd eq ufortran_eq)
    (concat start-delim ufortran_eq end-delim))))

(defun add-ufortran-formula ()
  "Runs ufortran to process the formula and converts it to proper Latex."
  (interactive)
  (move-end-of-line 1)
  (insert (concat "\n" (run-ufortran-on-line)))
  (move-beginning-of-line 1))
