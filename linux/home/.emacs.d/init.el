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
(set-frame-font "Office Code Pro 11")

;; disable electric-indent-mode always
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; ediff
(setq-default ediff-split-window-function 'split-window-horizontally)


;;
;; basic key bindings
;;

;; set "C-h" as delete-backward-char
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-?") (kbd "C-h"))
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
	:custom (winner-dont-bind-my-keys t)
	:defer t
	:commands (winner-undo winner-redo)
	:bind (("C-x p" . 'winner-undo)
				 ("C-x n" . 'winner-redo)))

;; recentf(builtin)
(use-package recentf
	:init (recentf-mode t)
	:defer t
	:config
	(setq recentf-max-saved-items 100)
	(setq recentf-exclude `("recentf" "ido.last" ,(expand-file-name package-user-dir)))
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

;; flymake(builtin)
(use-package flymake
	:bind (:map flymake-mode-map
							("C-, C-p" . flymake-goto-prev-error)
							("C-, C-n" . flymake-goto-next-error)
							("C-, C-c" . flymake-display-err-menu-for-current-line)))

;; org(builtin)
(use-package org
	:ensure t
	:mode ("\\.org$" . org-mode)
	:bind (("C-c c" . org-capture)
				 ("C-c a" . org-agenda)
				 ("C-c l" . org-store-link))
	:init
	(setq org-directory (expand-file-name "org" user-emacs-directory))
	(setq org-default-notes-file (expand-file-name "default-notes.org" org-directory))
	(setq org-agenda-files (list org-directory))
	(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
	
	;;(setq org-use-speed-commands t)
	(setq org-hide-leading-stars t)
	(setq org-return-follows-link t)
	(setq org-blank-before-new-entry
				'((heading . always)
					(plain-list-item . nil)))

	(setq org-todo-keywords
				'((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
	(setq org-log-done 'time)
	
	(setq org-capture-templates
				'(("n" "Note" entry (file+headline "notes.org" "notes") "* %?\n %U\n %i\n %a")
					("t" "Task" entry (file+headline "tasks.org" "tasks") "* TODO %?\n"))))

;; which-key
(use-package which-key
	:ensure t
	:hook (after-init . which-key-mode))

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
	:bind (("C-M-i" . company-complete)
				 :map company-active-map
				 ("C-n" . company-select-next)
				 ("C-p" . company-select-previous)
				 ("C-j" . company-complete-selection)
				 ("C-h" . nil))
	:config
	(setq company-idle-delay 0)
	(setq company-show-numbers nil)
	(setq company-tooltip-limit 20)
	(setq company-selection-wrap-around t))

;; anzu
(use-package anzu
	:ensure t
	:init (global-anzu-mode +1)
	:defer t
	:bind (("M-%" . anzu-isearch-query-replace)
				 ("C-M-%" . anzu-isearch-query-replace-regexp))
	:config
	(setq anzu-search-threshold 999))

(use-package yasnippet
	:ensure t
	:init (yas-global-mode 1)
	:config
	(setq yas-prompt-functions '(yas-ido-prompt)))

;; wgrep
(use-package wgrep
	:ensure t
	:defer t)

;; ggtags
(use-package ggtags
	:ensure t
	:defer t
	:hook (c-mode-common . (lambda ()
													 (when (derived-mode-p 'c-mode 'c++-mode)
														 (ggtags-mode 1)))))

;; projectile
(use-package projectile
	:ensure t
	:init (projectile-mode +1)
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map))
	:defer t)

;; lsp-mode
;; NOTE: Resuires language servers individually.
;;			 C/C++: pacman -S clang
;;			 python: pip install python-language-server (install pyls system wide)
;;			 golang: go get -u golang.org/saibing/bingo
(use-package lsp-mode
	:disabled
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
		(setq company-lsp-cache-candidates t)
		(setq company-lsp-enable-snippet t)
		(setq company-lsp-enable-recompletion t))
	;; lsp-ui
	(use-package lsp-ui
		:ensure t
		:hook ((lsp-mode . lsp-ui-mode))
		:bind (:map lsp-ui-mode-map
								([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
								([remap xref-find-references] . lsp-ui-peek-find-references); M-?
								("C-, C-m" . lsp-ui-imenu)
								("C-, C-i" . lsp-ui-find-implementation))
		:custom
		;; lsp-ui-doc
		(lsp-ui-doc-enable t)
		(lsp-ui-doc-header t)
		(lsp-ui-doc-include-signature t)
		(lsp-ui-doc-max-width 200)
		(lsp-ui-doc-max-height 60)
		(lsp-ui-doc-use-childframe t)
		;; lsp-ui-sideline
		(lsp-ui-sideline-enable nil)
		;; lsp-ui-peek
		(lsp-ui-peek-enable t)))

;; google-c-style
(use-package google-c-style
	:ensure t
	:defer t
	:hook (c-mode-common . google-set-c-style))

;; go-mode
;; NOTE: Requires "goimports"
;;			 go get -u golang.org/x/tools/cmd/goimports
(use-package go-mode
	:ensure t
	:defer t
	:hook (before-save . gofmt-before-save)
	:config
	(setq gofmt-command "goimports"))

;; neotree
(use-package neotree
	:ensure t
	:defer 1)


;;
;; theme config
;;
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
					 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))
(when (getenv "DISPLAY")
	;(load-theme 'solarized-dark t)
	(load-theme 'doom-Iosvkem t))


;;
;; windows customize
;;
(when (eq system-type 'windows-nt)
	;; font
	
	
	;; IME config
	(setq default-input-method "W32-IME")
	(setq-default w32-ime-mode-line-state-indicator "[--]")
	(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
	(w32-ime-initialize)

	;; python27 pyls
	(defun use-python27 ()
		(interactive)
		(setq lsp-clients-python-command '("c:\\python27\\scripts\\pyls.exe"))))


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
		(lsp-ui company-lsp neotree window-number wgrep use-package solarized-theme google-c-style ggtags company)))
 '(winner-dont-bind-my-keys t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )