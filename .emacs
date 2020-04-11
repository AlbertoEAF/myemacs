; -*- lexical-binding: t -*-

;;; .emacs -- emacs settings
(setq load-prefer-newer t)

;;; .emacs

;;; compiled with: ./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3

(setq dired-dwim-target t)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; (see https://blog.d46.us/advanced-emacs-startup/)
(setq gc-cons-threshold (* 100 1000 1000))

;(server-start); "default")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'req-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(cmake-tab-width 4)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 80)
 '(inhibit-startup-screen t)
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (cmake-mode yasnippet-snippets esup irony el-get req-package indent-tools yaml-imenu yaml-mode flycheck-yamllint flymake-yaml pdf-tools tramp spacemacs-theme solarized-theme kaolin-themes lispy rainbow-delimiters parinfer org-pomodoro request s language-detection fill-column-indicator projectile cargo flycheck-rust flymake-rust ob-rust racer rust-mode smex ag auctex sbt-mode helm-ag paredit terminal-here magit expand-region log4j-mode vlf ensime use-package bind-key dired-rsync sx ace-window avy evil htmlize epc company edit-server helm-youtube demo-it csv-mode hydra pandoc ox-pandoc unicode-fonts undo-tree smartscan multiple-cursors key-chord iy-go-to-char hideshow-org hide-mode-line hide-lines flycheck-clang-analyzer elpy ecb drag-stuff color-theme autopair auto-yasnippet auto-complete-clang-async auto-complete-clang ac-helm)))
 '(terminal-here-terminal-command (quote ("gnome-terminal"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq tramp-use-ssh-controlmaster-options nil) ; Lets TRAMP use my custom ssh:ControlPath

(require 'iso-transl)

;;(add-to-list 'load-path "~/myemacs") ;; already done at the ~/.emacs
(load "myemacs")
(load "move_lines")
(load "myfiles")
(load "my-rsync")
(load "cpp-config")


                                        ;(add-to-list 'load-path "~/myemacs/emacs-webkit")
                                        ;(load "webkit")

(require 'cc-mode)

(use-package autopair
  ;; diminish autopair mode to make a spacy mode line
  ;; :diminish autopair-mode
  :ensure t
  :config
  (autopair-global-mode 1)
  (setq autopair-autowrap t))



(use-package yasnippet
  :ensure t
  :disabled
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode 1)
  ;; Yasnippet with Shift+Tab
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  )

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;(custom-set-variables
; '(ac-auto-start nil))

;(ac-set-trigger-key "")
;(ac-set-trigger-key "<tab>")

(require 'auto-complete-clang)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)


;;(require 'ecb)
;(require 'ecb-autoloads)


;;; replacement for built-in ecb-deactive, ecb-hide-ecb-windows and
;;; ecb-show-ecb-windows functions
;;; since they hide/deactive ecb but not restore the old windows for me
(defun tmtxt/ecb-deactivate ()
  "deactive ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-deactivate)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun tmtxt/ecb-hide-ecb-windows ()
  "hide ecb and then split emacs into 2 windows that contain 2 most recent buffers"
  (interactive)
  (ecb-hide-ecb-windows)
  (split-window-right)
  (switch-to-next-buffer)
  (other-window 1))
(defun tmtxt/ecb-show-ecb-windows ()
  "show ecb windows and then delete all other windows except the current one"
  (interactive)
  (ecb-show-ecb-windows)
  (delete-other-windows))

;(global-set-key (kbd "C-x C-;") 'ecb-activate)
;(global-set-key (kbd "C-x C-'") 'tmtxt/ecb-deactivate)
;(global-set-key (kbd "C-;") 'tmtxt/ecb-show-ecb-windows)
;(global-set-key (kbd "C-'") 'tmtxt/ecb-hide-ecb-windows)

;(global-set-key (kbd "M-x") 'helm-M-x)

; DOXYMACS
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/doxymacs")
;; Doxymacs hooks
;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (require 'doxymacs)
;;     (doxymacs-mode t)
;;     (doxymacs-font-lock)))
;; (add-hook 'c++-mode-common-hook
;;   (lambda ()
;;     (require 'doxymacs)
;;     (doxymacs-mode t)
;;     (doxymacs-font-lock)))

(require 'helm-config)


;;(helm-mode 1)

;;(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)




;; scroll
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;(setq scroll-step 1) ;; keyboard scroll one line at a time

(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

(add-hook 'python-mode-hook
      (lambda ()
        (setq-default indent-tabs-mode nil)
        (setq-default tab-width 4)
        (setq-default python-indent 4)))


(require 'drag-stuff)
(drag-stuff-global-mode t)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;(ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-define-global "çi" 'indent-for-tab-command)
(key-chord-define-global "ç<" 'multiple-cursors-hydra/body)
(key-chord-define-global "çp" 'org-pomodoro)




(key-chord-mode 1)

;(key-chord-define-global "hj"     'undo)

(require 'iy-go-to-char)

;(key-chord-define-global "fg" 'iy-go-to-char)
;(key-chord-define-global "df" 'iy-go-to-char-backward)

;(key-chord-define-global "cj" 'backward-char)
;(key-chord-define-global "ck" 'next-line)
;(key-chord-define-global "cl" 'previous-line)
;(key-chord-define-global "cç" 'forward-char)
;
;(key-chord-define-global "wj" 'backward-word)
;(key-chord-define-global "wk" 'next-line)
;(key-chord-define-global "wl" 'previous-line)
;(key-chord-define-global "wç" 'forward-word)
;

(key-chord-define-global "çt" 'org-sparse-tree)
(key-chord-define-global "çd" 'duplicate-line)

(require 'expand-region)
(key-chord-define-global "ça" 'er/expand-region)

(add-hook 'after-init-hook #'global-flycheck-mode)



(put 'downcase-region 'disabled nil)


;(require 'hide-comnt)
(define-key global-map (kbd "C-c #") 'hide/show-comments-toggle)

(defun Kill-whole-line ()
  (interactive)
;  (previous-line)
  (let ((oldpoint (point)))
;    (forward-line)
    (kill-whole-line)
    (goto-char oldpoint)))



(define-key global-map (kbd "C-S-k") 'kill-whole-line)

(global-smartscan-mode 1)

(defun create-indent-and-go-to-next-line ()
  (interactive)
  (move-end-of-line 1)
  (insert "\n")
  (indent-for-tab-command))

(defun add-empty-line-above ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (create-indent-and-go-to-next-line)))


(define-key global-map (kbd "S-<return>") 'create-indent-and-go-to-next-line)
(define-key global-map (kbd "C-<return>") 'add-empty-line-above)


(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))


(define-key global-map (kbd "C-a") 'smart-line-beginning)
;(define-key global-map (kbd "M-g") 'keyboard-quit)

; Editing key-chords
(key-chord-define-global "ºe"  'move-end-of-line)
(key-chord-define-global "ºa"  'smart-line-beginning)
(key-chord-define-global "ºp"  'previous-line)
(key-chord-define-global "ºf"  'forward-char)
(key-chord-define-global "ºn"  'next-line)
(key-chord-define-global "ºb"  'backward-char)
(key-chord-define-global "ºl"  'recenter-top-bottom)
(key-chord-define-global "º'"  'isearch-forward)
(key-chord-define-global "ºc"  'select-org-table-cell)

;(key-chord-define-global "ºj"  'ace-jump-mode)



; Command actions key-chords
(key-chord-define-global "çm" 'magit-status)
;(key-chord-define-global "çb"  'helm-mini)
(key-chord-define-global "çb"  'ido-switch-buffer)
;(key-chord-define-global "çf"  'helm-find-files)
(key-chord-define-global "çf"  'ido-find-file)
(key-chord-define-global "çs"  'save-buffer)
(key-chord-define-global "ÇS"  'save-buffer)
(key-chord-define-global "ço"  'other-window)
(key-chord-define-global "çk"  'kill-buffer)
(key-chord-define-global "ÇK"  'clean-buffers)
(key-chord-define-global "çu"  'undo)
(key-chord-define-global "ç1"  'delete-other-windows)
(key-chord-define-global "ç2"  'split-window-below)
(key-chord-define-global "ç3"  'split-window-right)
(key-chord-define-global "ç0"  'delete-window)
(key-chord-define-global "çw"  'kill-word)
(key-chord-define-global "ç "  'whitespace-cleanup)
(key-chord-define-global "çj"  'join-line)
(key-chord-define-global "ÇJ"  'join-line)
(key-chord-define-global "çc"  'comment-or-uncomment-region)
(key-chord-define-global "çe"  'eval-buffer)
(key-chord-define-global "çr"  'my-rsync-from-dired-mark)
(key-chord-define-global "çn"  'create-and-go-to-next-line)
(key-chord-define-global "çh"  'replace-string)

(key-chord-define-global "ºj" 'avy-goto-word-or-subword-1)

                                        ; Experimental
                                        ;(key-chord-define-global "+h"  'helm-mini)



                                        ;(require 'undo-tree)
                                        ;(global-undo-tree-mode)


(put 'upcase-region 'disabled nil)

(set-frame-font "Inconsolata-16")


;; elpy
;(elpy-enable)
;; use flycheck not flymake with elpy
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))




;;(setq py-python-command "python3") ;; for python-mode.el's interpreter choice
;;(add-hook 'python-mode-hook #'(lambda () (setq py-python-command "python3")))
;;(setq python-shell-interpreter "python3")


;;; clock time
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


; org-mode inline languages and disable confirmation
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (shell . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-tab-acts-natively t) ; better editing behaviour for python indentation

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree "~/org/journal.org")
        "* %?\nEntered on %U\n  %i\n  %a")))


(setq x-select-enable-clipboard t)


(winner-mode 1)


(defun my/setup_dired_shortcuts ()
  (local-set-key (kbd "M-<up>") 'dired-up-directory))

(add-hook 'dired-mode-hook 'my/setup_dired_shortcuts)


(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))



(setq dired-listing-switches "-alh") ;; human-readable filesizes in dired


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))


;; (defun my-find-file-check-make-large-file-read-only-hook ()
;;   "If a file is over a given size, make the buffer read only."
;;   (when (> (buffer-size) (* 1024 1024))
;;     (setq buffer-read-only t)
;;     (buffer-disable-undo)
;;     (fundamental-mode)))

;(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)




(defun clean-buffers ()
  (interactive)
  (let ((clean-buffer-list-delay-general 0))
    (clean-buffer-list)))


(defun get-sql-file-corresponding-image ()
  (interactive)
  (let* ((filepath (buffer-file-name))
         (filename (substring filepath 0 -3))
         (img_path (concat filename "png")))
    (find-file-other-window img_path)
    )
  )


(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))


(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex))


(setq ido-create-new-buffer 'always)

(show-paren-mode 1)

(defun org-f5 ()
  (interactive)
  (save-buffer)
  (org-html-export-to-html))


(define-key org-mode-map (kbd "<f5>") 'org-f5)

;; (require 'kaolin-themes)
;; (load-theme 'kaolin-galaxy t)
;; (require 'solarized-theme)
;; (load-theme 'solarized-theme t)

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))


(use-package spaceline
  :demand t
  :defer t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))


(pdf-tools-install)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))



(load (expand-file-name "~/.roswell/helper.el"))

;; Make gc pauses faster by decreasing the threshold.
;; (see https://blog.d46.us/advanced-emacs-startup/)
(setq gc-cons-threshold (* 2 1000 1000))

(req-package-finish)
(provide '.emacs)
;;; .emacs ends here
