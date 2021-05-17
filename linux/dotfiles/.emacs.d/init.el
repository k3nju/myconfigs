;;(profiler-start 'cpu)

;; need to insert non-graphic chars?
(global-unset-key (kbd "C-q"))


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
(setq select-enable-clipboard t)
(setq select-enable-primary t)
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

;; lsp relateds tuning
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max 2097152) ;; 1024 * 1024 * 2


;;
;; basic key bindings
;;

;; set "C-h" as delete-backward-char, use F1 to see helps(default keybinding)
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
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
;; helpers
;;

(defun add-before-save-hook (f)
	(add-hook 'before-save-hook f nil 'local))


;;
;; Package config
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

;; auto-package-update
(use-package auto-package-update
	:disabled
	:ensure t
	:config
	(setq auto-package-update-delete-old-versions t)
	(auto-package-update-maybe))

;; uniquify(builtin)
(use-package uniquify
	:config (setq uniquify-buffer-name-style 'forward))

;; recentf(builtin)
(use-package recentf
	:config
	(recentf-mode t)
	(setq recentf-max-saved-items 100)
	(setq recentf-exclude `("recentf" "ido.last" ,(expand-file-name package-user-dir)))
	;; recentf-auto-save-timer deprecated?
	;;(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
	)

;; ido(builtin)
(use-package ido
	:config
	(ido-mode t)
	(setq ido-everywhere t)
	(setq ido-enable-flex-matching t)
	(setq ido-use-virtual-buffers t)
	(setq ido-use-filename-at-point 'guess)
	;; ido-vertical-mode
	(use-package ido-vertical-mode
		:ensure t
		:config
		(ido-vertical-mode t)
		(setq ido-vertical-show-count t)
		(setq ido-vertical-define-keys 'C-n-and-C-p-only))
	;; ido-completing-read+(ido-ubiquitous)
	(use-package ido-completing-read+
		:ensure t
		:config (ido-ubiquitous-mode t))
	;; amx
	(use-package amx
		:ensure t
		:bind (("M-x" . amx))
		:config (amx-mode)
		:custom
		(amx-backend 'ido)))

;; org(builtin)
(use-package org
	:ensure t
	:bind (("C-c c" . org-capture)
				 ("C-c a" . org-agenda)
				 ("C-c l" . org-store-link))
	:custom
	(org-display-custom-times t) ;; or use setq-default in config
	(org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M:%S>"))
	
	:config
	(setq org-directory (expand-file-name "org" user-emacs-directory))
	(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
	(setq org-agenda-files (list org-directory))
	(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

	;;(setq org-use-speed-commands t)
	(setq org-hide-leading-stars t)
	(setq org-adapt-indentation t)
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

;; winner(builtin)
(use-package winner
	:config (winner-mode 1)
	:custom (winner-dont-bind-my-keys t)
	:bind (("C-x p" . 'winner-undo)
				 ("C-x n" . 'winner-redo)))

;; window-number
(use-package window-number
	:ensure t
	;; *not work* :init (window-number-meta-mode)
	:config (window-number-meta-mode))

;; which-key
(use-package which-key
	:ensure t
	:hook (after-init . which-key-mode))

;; goto-chg
(use-package goto-chg
	:ensure t
	:bind (("C-q p" . goto-last-change)
				 ("C-q n" . goto-last-change-reverse)))

;; company
(use-package company
	:ensure t
	:bind (("C-M-i" . company-complete)
				 :map company-active-map
				 ("C-n" . company-select-next)
				 ("C-p" . company-select-previous)
				 ("C-j" . company-complete-selection)
				 ("C-h" . nil))
	:config
	(global-company-mode)
	(setq company-idle-delay 0)
	(setq company-show-numbers nil)
	(setq company-tooltip-limit 20)
	(setq company-selection-wrap-around t))

;; anzu
(use-package anzu
	:ensure t
	:bind (("M-%" . anzu-isearch-query-replace)
				 ("C-M-%" . anzu-isearch-query-replace-regexp))
	:config
	(global-anzu-mode +1)
	(setq anzu-search-threshold 999))

;; yasnippet
(use-package yasnippet
	:ensure t
	:config
	;; actual snippets
	(use-package yasnippet-snippets
		:ensure t)
	(yas-global-mode t)
	(yas-reload-all)
	(setq yas-prompt-functions '(yas-ido-prompt)))

;; wgrep
(use-package wgrep
	:ensure t)

;; ggtags
(use-package ggtags
	:ensure t
	:bind (("C-q g" . 'ggtags-mode))
	;:hook (c-mode-common . (lambda ()
	;(when (derived-mode-p 'c-mode 'c++-mode)
	;(ggtags-mode 1)))))
)

;; projectile
(use-package projectile
	:ensure t
	:init
	(projectile-mode +1)
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map)))

;; treemacs
(use-package treemacs
	:ensure t
	:bind ("C-q t" . treemacs))

;; treemacs-projectile
(use-package treemacs-projectile
	:ensure t
	:after treemacs projectile)

;; flycheck
(use-package flycheck
	:ensure t
	:config (global-flycheck-mode))

; flymake(builtin)
(use-package flymake
	:disabled
	:bind (:map flymake-mode-map
							("C-q C-p" . flymake-goto-prev-error)
							("C-q C-n" . flymake-goto-next-error)))

;; lsp-mode
;; NOTE: Resuires language servers individually.
;;			 C/C++: pacman -S clang
;;			 python: pip install python-language-server (install pyls system wide)
;;			 golang: go get golang.org./x/tools/gopls@latest
(use-package lsp-mode
	:ensure t
	:commands lsp-deferred
	:hook ((prog-mode . lsp-deferred))
	:custom
	(lsp-log-io nil) ; t when debug
	(lsp-prefer-flymake nil)
	(lsp-keymap-prefix "C-q l")
	(lsp-signature-auto-activate nil)
	(lsp-completion-provider :capf)
	:config
	(setq lsp-clients-clangd-args '("-j=2" "--background-index" "--log=error"))
	
	;; lsp-ui
	(use-package lsp-ui
		:ensure t
		:commands lsp-ui-mode
		:hook ((lsp-mode . lsp-ui-mode))
		:bind (:map lsp-ui-mode-map
								([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
								([remap xref-find-references] . lsp-ui-peek-find-references) ; M-?
								("C-q C-u m" . lsp-ui-imenu)
								("C-q C-u f i" . lsp-ui-find-implementation))
		:custom
		(lsp-lens-enable t)
		
		;; lsp-ui-doc
		(lsp-ui-doc-enable nil)
		(lsp-ui-doc-header t)
		(lsp-ui-doc-include-signature t)
		(lsp-ui-doc-delay 2)
		(lsp-ui-sideline-show-code-actions t)

		;; lsp-ui-sideline
		(lsp-ui-sideline-enable t)
		(lsp-ui-sideline-delay 0.2)
		(lsp-ui-sideline-show-hover nil)
		(lsp-ui-sideline-show-diagnostics t)

		;; lsp-ui-peek
		(lsp-ui-peek-always-show t)

		:custom-face
		(lsp-ui-sideline-symbol-info ((t (:background "default"))))
		;; background face of sideline and doc
		(markdown-code-face ((t (:background "grey10"))))))

;; cc-mode(builtin)
(use-package cc-mode
	:ensure t
	:config
	;; google-c-style
	(use-package google-c-style
		:ensure t
		:hook (c-mode-common . google-set-c-style))

	;; clang-format
	(use-package clang-format
		:ensure t
		:bind (("C-q f b" . clang-format-buffer)
					 ("C-q f r" . clang-format-region))
		:hook (c-mode-common . (lambda () (add-before-save-hook 'clang-format-buffer)))
		:custom
		(clang-format-style "file")
		(clang-format-fallback-style "google")))

;; go-mode
(use-package go-mode
	:ensure t
	:after lsp-mode
	:hook (go-mode . (lambda ()
										 (add-before-save-hook
											(lambda ()
												(lsp-format-buffer)
												(lsp-organize-imports))))))

;; csharp-mode
(use-package csharp-mode
	:ensure nil
	:config
	(setq-default tab-width 4)
	(setq-default c-basic-offset 4)
	(setq tab-stop-list (number-sequence 4 120 4)))

;;
;; theme config
;;
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
					 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))
(when window-system
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" default))
 '(package-selected-packages
	 '(yasnippet-snippets gnu-elpa-keyring-update csharp-mode treemacs-projectile amx powershell hcl-mode yasnippet window-number which-key wgrep use-package treemacs projectile lsp-ui ido-vertical-mode ido-completing-read+ google-c-style go-mode ggtags flycheck doom-themes clang-format anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-symbol-info ((t (:background "default"))))
 '(markdown-code-face ((t (:background "grey10")))))


;;(profiler-report)
;;(profiler-stop)
