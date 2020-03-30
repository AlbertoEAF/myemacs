;; Programmer: Alberto Ferreira
;; Date of release: 11/10/2013
;; License: GPLv2 Code

;;; http://ergoemacs.org/emacs/elisp_all_about_lines.html

;; (defun line-start ()
;;   "Returns the position of the line start in the buffer"
;;   (interactive)
;;   (save-excursion
;;     (move-beginning-of-line 1)
;;     (point)))

;; (defun line-end ()
;;   "Returns the position of the line end in the buffer"
;;   (interactive)
;;   (save-excursion
;;     (move-end-of-line 1)
;;     (point)))



(defun get-line ()
  "Returns the current line content"
  (interactive)
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun print-line ()
  "Prints the current line contents"
  (interactive)
  (message "%s" (get-line)))


;; (defun range (start end)
;;   "Shows the range selected"
;;   (interactive "r")
;;   (message "Text is selected from %d to %d" start end))

;(current-buffer)
;(other-buffer)
;(switch-to-buffer (other-buffer))
;(print (point-min) (point-max))(point)(point)
