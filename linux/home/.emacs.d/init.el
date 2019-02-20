;; references
;; https://github.com/technomancy/better-defaults/blob/master/better-defaults.el


;;
;; coding system
;;

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)


;;
;; basic configs
;;

;; display
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; disable bell & screen flashes
(setq ring-bell-function 'ignore)

;; line and column number
(if (>= emacs-major-version 26)
    (global-display-line-numbers-mode t)
  (global-linum-mode t))
(setq linum-format "%4d ")
(column-number-mode t)

;; cursor
(blink-cursor-mode 0)
(global-subword-mode t)
(global-hl-line-mode t)

;; buffer
(show-paren-mode t)
(setq show-trailing-whitespace t)

;; copy & paste
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; search
(setq case-fold-search t)

;; tab
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq tab-always-indent 'complete)

;; line trancation
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)

;; path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; backup
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; font & color
(set-frame-font "OfficeCodePro 11")
;(setq font-lock-comment-delimiter-face ((t (:foreground "#008b00"))))
;(setq font-lock-comment-face ((t (:foreground "#008b00"))))
;(setq font-lock-string-face ((t (:foreground "#a42c00")))))

;; disable electric-indent-mode always
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; ediff
(setq-default ediff-split-window-function 'split-window-horizontally)


;;
;; key bindings
;;

;; set "C-h" as delete-backward-char
(global-set-key (kbd "C-h") 'delete-backward-char)
;; enable cursor to move with M-p and M-p among windows
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") '(lambda ()
			       (interactive)
			       (other-window -1)))
;; reverse isearch and regex isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;;
;; package config
;;

;; package
(when (require 'package nil t)
  (package-initialize)
  (setq package-archives
		    '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("melpa" . "http://melpa.org/packages/")
		      ("org" . "http://orgmode.org/elpa/"))))

;; use-package
(unless (package-installed-p 'use-package)
  (message "use-package is not installed and installing it") 
  (package-refresh-contents)
  (package-install 'use-package))

;; uniquify(builtin)
(use-package uniquify
	:defer t
  :config (setq uniquify-buffer-name-style 'forward))

;; winner-mode(builtin)
(use-package winner
  :init (winner-mode 1)
	:defer t
	:commands (winner-undo winner-redo)
	:bind (("C-x p" . 'winner-undo)
				 ("C-x n" . 'winner-redo)))

;; ido(builtin)
(use-package ido
  :init (ido-mode t)
	:defer t
  :config
  (setq ido-enable-flex-matching t)
  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode t)))

;; window-number
(use-package window-number
  :ensure t
	;; *not work* :init (window-number-meta-mode)
	:config (window-number-meta-mode))

;; wgrep
(use-package wgrep
  :ensure t
  :defer t)

;; neotree
(use-package neotree
  :ensure t
  :defer 1)

;; anzu
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :bind (("M-%" . anzu-isearch-query-replace)
	 ("C-M-%" . anzu-isearch-query-replace-regexp)))

;; company
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
	(global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (setq company-idle-delay 0)
  (setq company-show-numbers t))

;; ggtags
(use-package ggtags
  :ensure t
  :defer 1
  :init (add-hook 'c-mode-common-hook
									(lambda ()
										(when (derived-mode-p 'c-mode 'c++-mode)
											(ggtags-mode 1)))))

;; projectile
(use-package projectile
	:ensure t
	:defer 1
	:config
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(projectile-mode +1))

;; google-c-style
(use-package google-c-style
  :ensure t
	:init (add-hook 'c-mode-common-hook 'google-set-c-style))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
	:hook ((prog-mode . lsp))
	:config
	(use-package company-lsp
		:ensure t
		:after company
		:config
		(add-to-list 'company-backends 'company-lsp)))

;; lsp-ui
;(use-package lsp-ui
;  :commands lsp-ui-mode)

;; theme config
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
           (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))
(when (getenv "DISPLAY")
  (load-theme 'solarized-dark t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(company-lsp neotree window-number wgrep use-package solarized-theme google-c-style ggtags company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
