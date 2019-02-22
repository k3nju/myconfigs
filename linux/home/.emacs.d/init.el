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
(setq kept-old-versions 0)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; font & color
(set-frame-font "OfficeCodePro 11")

;; disable electric-indent-mode always
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; ediff
(setq-default ediff-split-window-function 'split-window-horizontally)


;;
;; basic key bindings
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

;; winner(builtin)
(use-package winner
	:init (winner-mode 1)
	:defer t
	:commands (winner-undo winner-redo)
	:bind (("C-x p" . 'winner-undo)
				 ("C-x n" . 'winner-redo)))

;; recentf(builtin)
(use-package recentf-mode
	:init (recentf-mode t)
	:defer t
	:config
	(setq recentf-max-saved-items 100)
	(setq recentf-exclude '(".recentf"))
	(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)))

;; ido(builtin)
(use-package ido
	:init (ido-mode t)
	:defer t
	:config
	(setq ido-everywhere t)
	(setq ido-enable-flex-matching t)
	(setq ido-use-virtual-buffers t)
	;; ido-vertical-mode
	(use-package ido-vertical-mode
		:ensure t
		:init (ido-vertical-mode t)
		:config
		(setq ido-vertical-show-count t)
		(setq ido-vertical-define-keys 'C-n-and-C-p-only))
	;; ido-completing-read+(ido-ubiquitous)
	(use-package ido-completing-read+
		:ensure t
		:init (ido-ubiquitous-mode t)))

;; window-number
(use-package window-number
	:ensure t
	;; *not work* :init (window-number-meta-mode)
	:config (window-number-meta-mode))

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
	(setq company-show-numbers nil)
	(setq company-tooltip-limit 20)
	(setq company-selection-wrap-around t))

;; anzu
(use-package anzu
	:ensure t
	:init (global-anzu-mode +1)
	:bind (("M-%" . anzu-isearch-query-replace)
	 ("C-M-%" . anzu-isearch-query-replace-regexp)))

;; ggtags
(use-package ggtags
	:ensure t
	:defer t
	:init (add-hook 'c-mode-common-hook
									(lambda ()
										(when (derived-mode-p 'c-mode 'c++-mode)
											(ggtags-mode 1)))))

;; wgrep
(use-package wgrep
	:ensure t
	:defer t)

;; neotree
(use-package neotree
	:ensure t
	:defer 1)

;; projectile
(use-package projectile
	:ensure t
	:defer t
	:config
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(projectile-mode +1))

;; google-c-style
(use-package google-c-style
	:ensure t
	:defer t
	:init (add-hook 'c-mode-common-hook 'google-set-c-style))

;; lsp-mode
;; NOTE: Resuires language servers individually.
;;			 C/C++: pacman -S clang
;;			 python: pip install python-language-server (install pyls system wide)
;;			 golang: go get -u golang.org/saibing/bingo
(use-package lsp-mode
	:ensure t
	:defer t
	:commands lsp
	:hook ((prog-mode . lsp))
	:config
	;; company-lsp
	(use-package company-lsp
		:ensure t
		:after company
		:config
		(add-to-list 'company-backends 'company-lsp)
		(setq company-lsp-enable-snippet t)
		(setq company-lsp-enable-recompletion t))
	;; lsp-ui
	(use-package lsp-ui
		:ensure t
		:hook ((lsp-mode . lsp-ui-mode))))

;; go-mode
;; NOTE: Requires "goimports"
;;			 go get -u golang.org/x/tools/cmd/goimports
(use-package go-mode
	:ensure t
	:defer t
	:config
	(setq gofmt-command "goimports")
	(add-hook 'before-save-hook 'gofmt-before-save))


;;
;; theme config
;;
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
					 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))
(when (getenv "DISPLAY")
	(load-theme 'solarized-dark t))


;;
;; customize
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(lsp-ui company-lsp neotree window-number wgrep use-package solarized-theme google-c-style ggtags company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
