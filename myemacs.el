;;;; INSTRUCTIONS
;;;;   Add this lines to your .emacs file, being myemacs the folder where you saved the files:
;; (add-to-list 'load-path "~/Dropbox/myemacs")
;; (load "myemacs")

;;; MELPA package repository
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(require 'color-theme)

;;require 'color-theme-solarized)
;;color-theme-solarized-dark)
;;load-theme 'solarized-dark t)
;;load-theme 'solarized-light t)

;;load-theme 'zenburn t)

;; Slime
;;setq inferior-lisp-program "/usr/bin/sbcl")
;;add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
;;require 'slime)

;;slime-setup)

(defun create-and-go-to-next-line ()
  (interactive)
  (move-end-of-line nil)
  (autopair-newline))

(defun goto-next-dot ()
  (interactive)
  (search-forward "."))
(defun goto-prev-dot ()
  (interactive)
  (search-backward "."))
(defun goto-next-comma ()
  (interactive)
  (search-forward ","))
(defun goto-prev-comma ()
  (interactive)
  (search-backward ","))
(defun goto-next-dollar ()
  (interactive)
  (search-forward "$"))
(defun goto-prev-dollar ()
  (interactive)
  (search-backward "$"))



;(eval-after-load "autopair-autoloads" ; <-- "autopair-autoloads" not "autopair"
;  '(progn
;     (require 'autopair)
;     (autopair-global-mode 1)
;     ))

; my own commands
(put 'narrow-to-region 'disabled nil)
(setq tex-dvi-view-command "xdvi")
;(load "smooth-scrolling")
;(require 'smooth-scrolling)

(global-set-key (kbd "<f5>") 'pdflatex)
(global-set-key (kbd "S-<f5>") 'pdflatex+bib)
(global-set-key (kbd "<f6>") 'set-main-tex)
(global-set-key (kbd "<f7>") 'jump-to-main-tex)
(global-set-key (kbd "<f9>") 'add-ufortran-formula)

(global-set-key (kbd "<f12>") 'latex-warning)

(global-set-key (kbd "C-.") 'goto-next-dot)
(global-set-key (kbd "C-:") 'goto-prev-dot)
;(global-set-key (kbd "C-,") 'goto-next-comma)
;(global-set-key (kbd "C-;") 'goto-prev-comma)
;;(global-set-key (kbd "C--") 'goto-next-dollar)
;;(global-set-key (kbd "C-_") 'goto-prev-dollar)

(global-set-key (kbd "<f10>") 'latex-power)
(global-set-key (kbd "S-<f10>") 'latex-power-parenthesis)

;(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)

(global-set-key (kbd "C-x p") 'pdflatex-open-pdf)

(global-set-key (kbd "C-S-f") 'latex-select-frac)

(global-set-key (kbd "s-r") 'recentf-open-files)
(global-set-key (kbd "s-o") 'myfiles)

;; Org-mode settings
(use-package org
  :defer t
  :init
  (setq org-agenda-files (list
                          "~/Documents/leumi/leumicard-ds"
                          "~/org"
                          "~/org/leumi"
                          "~/org/cb"))

  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-font-lock-mode 1)

(require 'ox-gfm nil t)
(load-file "~/myemacs/org-gitbook.el")


)

(use-package ox-reveal
  :after org
  :init
  (add-to-list 'load-path "~/code/external/org-reveal/ox-reveal.el")
  (load-file "~/code/external/org-reveal/ox-reveal.el")
  (require 'ox-reveal)
  (setq org-reveal-root "file:///home/alberto.ferreira/code/external/reveal.js/"))





;; CUDA
                                        ;(add-to-list 'load-path "~/Dropbox")
                                        ;(autoload 'cuda-mode "cuda-mode.el")
                                        ;(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

                                        ;(load "auctex.el" nil t t)


(load "utils")
(load "line_functions")
(load "ufortran_extension")
(load "latex-extensions")
(load "duplicate_line")



(global-set-key (kbd "C-S-d") 'duplicate-line)

(global-set-key (kbd "s-b") 'latex-add-begin-end)

;(byte-compile-file "utils.el")
;(byte-compile-file "line_functions.el")
;(byte-compile-file "ufortran_extension.el")
;(byte-compile-file "latex.el")


;; Disable the toolbar
(tool-bar-mode -1)

;; Disable menu-bar
(menu-bar-mode -1)

;; Disable the scrollbar
(scroll-bar-mode -1)

;(desktop-save-mode 1)

;; Enable line numbers on the left
(global-linum-mode t)
;; Enable column number on the status bar
(column-number-mode 1)

;; Disable fringes
(set-fringe-mode 1)

;; Enable recent files mode
(recentf-mode 1)


;; Highlight content inside parenthesis
;(show-paren-mode 1)
;(setq show-paren-style 'expression)

;; Standard text editor behaviour (cut,copy,paste,undo)
;(cua-mode 1)


(blink-cursor-mode 0)

(defhydra multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))







;; Latex - Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)


;; Shell call typer.py

(defun typer-paste-clipboard ()
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  ;(shell-command "xdotool keydown alt key Tab; sleep 0.1; xdotool keyup alt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.6"))


(defun typer-paste-clipboard-remove-cols-csv-line ()
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  ;(shell-command "xdotool keydown alt key Tab; sleep 0.1; xdotool keyup alt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.5 -d,"))


(defun typer-paste-region ()
  "Pastes an emacs region already in the clipboard. Assumes Unix newline-style line-endings."
  (interactive)
  (message "Running typer paste clipboard...")
  (shell-command "setxkbmap pt")
  (async-shell-command "python2 ~/code/pystash/typer.py -s 5 -m clip --sleep-post-action 0.5 -d'\n' --sleep-per-char 0.01"))





(setq ring-bell-function 'ignore)

(defun select-org-table-cell ()
  "Selects a cell content in an org table if the cursor is placed inside"
  (interactive)
  (search-backward "|")
  (forward-char)
  (skip-chars-forward " ")
  (set-mark (point))
  (search-forward "|")
  (backward-char)
  (skip-chars-backward " "))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  )

(require 'fill-column-indicator)
(setq fci-rule-column 100)



;;;; JIRA
;; (setq jiralib-url "https://issues.xxxx.com/");secure/Dashboard.jspa")
;; (use-package ejira
;;   :load-path "~/code/external/ejira"
;;   :ensure    nil
;;   :init
;;   (setq jiralib2-url             "https://issues.xxxxx.com/"
;;         jiralib2-user-login-name "xxxxxxx"
;;         ejira-projects           '("xxxx", "xx")
;;         ejira-main-project       "xxxx"
;;         ejira-my-org-directory   "~/org_ejira"
;;         ejira-done-states        '("Done")
;;         ejira-in-progress-states '("In Progress" "In Review" "Testing")
;;         ejira-high-priorities    '("High" "Highest")
;;         ejira-low-priorities     '("Low" "Lowest")
;;         ))
;;         ;; Customize these based on your JIRA server configuration
;;         ;;ejira-sprint-field                     'customfield_10001
;;         ;;ejira-epic-field                       'customfield_10002
;;         ;;ejira-epic-summary-field               'customfield_10004))



;;;; LISP ;;;;

                                        ;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;;(setq inferior-lisp-program "sbcl")

(use-package slime
  :ensure t
  :pin melpa
  :defer t
  :commands slime
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q run")
  
  ;;(add-to-list 'load-path "path/of/slime")
  (require 'slime-autoloads)

  (load "~/.roswell/lisp/quicklisp/log4slime-setup.el")
  (global-log4slime-mode 1)

  (key-chord-define-global "çq" 'slime-eval-last-expression)
  )

;;; emacs parinfer
(use-package parinfer
  :ensure t
  :defer 1
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))


(load "sweet-lisp.el")


(provide 'myemacs)
;;; myemacs ends here

