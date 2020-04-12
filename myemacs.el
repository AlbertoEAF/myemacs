(require 'iso-transl)

;;(add-to-list 'load-path "~/myemacs") ;; already done at the ~/.emacs
(load "myemacs")
;; (load "cpp-config") ; broken with updates

(use-package cc-mode :ensure t)


;;(use-package auto-complete-config :ensure t)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)

;(custom-set-variables
; '(ac-auto-start nil))

;(ac-set-trigger-key "")
;(ac-set-trigger-key "<tab>")

(use-package 'auto-complete-clang :ensure t)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-clang)


;;(use-package ecb :ensure t)
;(use-package ecb-autoloads :ensure t)


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
;;     (use-package doxymacs :ensure t)
;;     (doxymacs-mode t)
;;     (doxymacs-font-lock)))
;; (add-hook 'c++-mode-common-hook
;;   (lambda ()
;;     (use-package doxymacs :ensure t)
;;     (doxymacs-mode t)
;;     (doxymacs-font-lock)))

(use-package helm-config :ensure t)


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







;(ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-define-global "çi" 'indent-for-tab-command)
(key-chord-define-global "ç<" 'multiple-cursors-hydra/body)







(add-hook 'after-init-hook #'global-flycheck-mode)



(put 'downcase-region 'disabled nil)



(global-smartscan-mode 1)


(key-chord-define-global "çn"  'create-and-go-to-next-line)


(set-frame-font "Inconsolata-16")



(defun get-sql-file-corresponding-image ()
  (interactive)
  (let* ((filepath (buffer-file-name))
         (filename (substring filepath 0 -3))
         (img_path (concat filename "png")))
    (find-file-other-window img_path)))




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


;; Make gc pauses faster by decreasing the threshold.
;; (see https://blog.d46.us/advanced-emacs-startup/)
(setq gc-cons-threshold (* 2 1000 1000))

(req-package-finish)
(provide '.emacs)
;;; .emacs ends here

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






(load "latex-utils")
(load "line_functions")
(load "ufortran_extension")
(load "latex-extensions")


(global-set-key (kbd "s-b") 'latex-add-begin-end)


;;(load "auctex.el" nil t t)
;; Latex - Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
