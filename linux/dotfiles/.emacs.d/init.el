;;(profiler-start 'cpu)

;; workaround
(setq byte-compile-warnings '(cl-functions))


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

;; for resizing *Completions* buffer size
(temp-buffer-resize-mode)

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

;; buffer
(show-paren-mode t)
(setq show-trailing-whitespace t)

;; find-file
(setq find-file-visit-truename t)

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

;; load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; backup
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 0)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))

;; auto save
;;(setq kill-buffer-delete-auto-save-files t)
;;(setq auto-save-timeout 5)

;; yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; font
;;(set-frame-font "Input 12" nil t)
;;(set-frame-font "Office Code Pro 11")
;;(set-frame-font "Noto Sans Mono CJK JP 10" nil t)
(when (and (eq window-system 'x) (eq system-type 'gnu/linux))
	(let* ((mappings '((:font "input mono condensed"
											:charset ascii)
										 (:font "ipaexgothic"
											:charset japanese-jisx0213.2004-1
											;; to supress flicking
											:rescale 0.999)))
				 ;; which should I use?
				 ;;(_myfontset (create-fontset-from-ascii-font
				 ;;							(format "%s-%d:weight=normal:slant=normal" "input mono condensed" 15) nil "myfontset"))
				 (myfontset (create-fontset-from-fontset-spec
										 (font-xlfd-name
											(font-spec :name "myfontset"
																 :weight 'normal
																 :slant 'normal
																 :width 'condensed
																 :size 13.0 ;; use floating point for point size.
																 :spacing 'M
																 :registry "fontset-myfontset")))))
		(mapc
		 (lambda (m)
			 (let* ((font (plist-get m :font))
							(charset (plist-get m :charset))
							(rescale (plist-get m :rescale))
							(fspec (font-spec :name font)))
				 (when (find-font fspec)
					 (set-fontset-font myfontset charset fspec nil 'append))
				 (when rescale
					 (add-to-list 'face-font-rescale-alist (cons (format ".*%s.*" font) rescale)))))
		 mappings)
		
		(add-to-list 'default-frame-alist '(font . "fontset-myfontset"))))
		
		;;(add-to-list 'face-font-rescale-alist '(".*ipaexgothic.*" . 0.999))))
		

(when (and (eq window-system 'w32) (eq system-type 'windows-nt))
	;; in windows, not "input mono condensed", use CamelCase
	;;(set-frame-font "InputMonoCondensed")
	(set-frame-font "MS Gothic 12" nil t)) ;; workaround

;; frame sizing and positioning
(when window-system
	(dolist (v '((width . 160)
							 (height . 50)
							 (top . 0)
							 ;;(left . (- 1000)) worked but doen't make mush sense
							 ))
		(add-to-list 'default-frame-alist v)))


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

;; need to insert non-graphic chars?
(global-unset-key (kbd "C-q"))
;; keep on coding
(global-unset-key (kbd "C-z"))
;; zenkaku hankaku
(setq default-input-method "japanese")
(global-unset-key (kbd "C-\\"))
(global-set-key
 (kbd "<zenkaku-hankaku>")
 #'(lambda ()
		 (interactive)
		 ;; to enable mozc overlay candidate drawing,
		 ;; truncate-lines must be enabled.
		 ;;(toggle-truncate-lines)
		 (toggle-input-method)))
	 

;; set "C-h" as delete-backward-char, use F1 to see helps(default keybinding)
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
;; enable cursor to move with M-p and M-p among windows
(global-set-key (kbd "M-n") #'other-window)
(global-set-key (kbd "M-p") #'(lambda () (interactive) (other-window -1)))

;; swap search key bindings
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)


;;
;; helpers
;;

(defun add-before-save-hook (f)
	(add-hook #'before-save-hook f nil 'local))


;;
;; Package config
;;

;; package
(when (require 'package nil t)
	(package-initialize)
	(setq package-archives
				'(("gnu" . "https://elpa.gnu.org/packages/")
					("melpa" . "https://melpa.org/packages/")
					("org" . "https://orgmode.org/elpa/"))))

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

;; hexl-mode(builtin)
(use-package hexl
	:config
	(setq hexl-bits 8))

;; uniquify(builtin)
(use-package uniquify
	:config	(setq uniquify-buffer-name-style 'forward))

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
;; NOTE: emacs28 has fido-vertical-mode.
;;       https://www.manueluberti.eu/emacs/2021/08/06/fido/
;;       consider this insted ido.
(use-package ido
	:config
	(ido-mode t)
	(setq ido-everywhere t)
	(setq ido-enable-flex-matching t)
	(setq ido-use-virtual-buffers t)
	(setq ido-use-filename-at-point 'guess)
	(setq ido-enable-regexp t)

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
		:bind ("M-x" . amx)
		:config	(amx-mode)
		;; unworked :init (amx-backend 'ido) and :config (amx-backend 'ido)
		:custom	(amx-backend 'ido)))

;; org(builtin)
(use-package org
	:ensure t
	:init
	;; must be set before org-mode loaded
	(setq org-display-custom-times t)
	:bind
	(("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("M-," . org-mark-ring-goto)
	 :map org-mode-map
	 ;; on terminal, "C-," is recognized to ",". on GUI works well.
	 ;; "C-c ," is default bound to org-priority, but change it to org-insert-structure-template
	 ;; for usability terminal and GUI
	 ("C-c ," . org-insert-structure-template))

	:custom-face
	;; :extend uneffected?
	(org-level-1 ((t (:extend t :underline t :weight ultra-bold :height 1.5))))
	(org-level-2 ((t (:weight bold :height 1.3))))
	(org-level-3 ((t (:weight bold :height 1.1))))

	:hook
	(org-capture-after-finalize . (lambda ()
																	;; HACK: if misc template invoked and that is aborted, delete a note file
																	(let* ((is-misc (org-capture-get :misc-note))
																				 (note-file-name (buffer-file-name (org-capture-get :buffer))))
																		(if (and is-misc
																						 org-note-abort
																						 (file-exists-p note-file-name))
																				(delete-file note-file-name)))))

	:config
	;; org-directory precedence
	;; 1) ~/Dropbox/org/
	;; 2) ~/.emacs.d/org/
	;; 3) ~/org/ (org-directory default)
	(mapc (lambda (v)
					(let* ((d (expand-file-name "org/" v)))
						(if (file-exists-p d) (setq org-directory d))))
				;; priority in reverse order
				(list user-emacs-directory "~/Dropbox/"))
	;; directory for misc notes
	(setq misc-notes-directory (expand-file-name "misc" org-directory))
	
	(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
	(setq org-agenda-files (list org-directory misc-notes-directory))
	(setq org-agenda-custom-commands
				'(("j" "[J]ournals" tags "+journal&+LEVEL=2&+TIMESTAMP>=\"<-1m>\""
					 ;; limit the files to be searched to org-default-notes-file
					 ((org-agenda-files (list org-default-notes-file))))
					("m" "[M]isc notes" tags "+misc"
					 ((org-agenda-files (list misc-notes-directory))))
					("1" "all level [1] headings" tags "LEVEL=1")
					("p" "[P]rojects" tags "+project&+LEVEL=1")))
	(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
	(setq org-refile-use-outline-path 'full-file-path)
	(setq org-refile-allow-creating-parent-nodes 'confirm)
	(setq org-outline-path-complete-in-steps nil)
	
	(setq org-archive-location (format "%s/%%s_archive::" (expand-file-name "archive" org-directory)))

	;;(setq org-use-speed-commands t)
	
	(setq org-startup-folded nil)
	(setq org-startup-indented t)
	(setq org-indent-indentation-per-level 1)
	;;(setq org-hide-leading-stars t)
	;;(setq org-adapt-indentation t) ;; hard indentation
	
	(setq org-return-follows-link t)
	(setq org-hide-emphasis-markers t)
	(setq org-blank-before-new-entry
				'((heading . always)
					(plain-list-item . nil)))
	(setq org-cycle-separator-lines 1)

	;; to enable time stamp overlay, set org-display-custom-times is t in :init
	(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M:%S>"))
	
	(setq org-todo-keywords
				'((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
	(setq org-log-done 'time)


	;; file target: create a note file
	(defun create-misc-note-file ()
		(interactive)
		(mkdir misc-notes-directory t)
		(let* ((get-filename (lambda ()
													 (expand-file-name
														(format "%s_%s.org"
																		(format-time-string "%Y-%m-%d")
																		(read-string "Misc note name: "))
														misc-notes-directory)))
					 (filename (funcall get-filename)))
			(while (file-exists-p filename)
				(setq filename (funcall get-filename)))
			filename))

	(setq org-capture-templates
				'(;;("n" "Notes" entry (file+headline "notes.org" "notes") "* %?\n%T\n" :empty-lines 1)
					("n" "[N]otes" entry (file "notes.org") "* %?\n%T\n" :empty-lines 1 :kill-buffer 1)
					;; NOTE: plain cant refile to other org files
					("m" "[M]isc notes" plain (file create-misc-note-file) "* %?  :misc:\n%T\n"
					 :misc-note t :empty-lines 1 :kill-buffer t)
					("j" "[J]ournals" entry (file+headline "notes.org" "journals") "* %T %?\n" :empty-lines 1 :kill-buffer t :prepend t)
					("d" "[D]iary" entry (file "diary.org") "* %T\n%?\n" :empty-lines 1 :kill-buffer t :prepend t )))
	
	(setq org-src-tab-acts-natively t)
	(setq org-src-preserve-indentation t)
	
	;; enable underline to EOL on headings
	(setq org-fontify-whole-heading-line t)

	;; org-id
	(use-package org-id
		;; org-id is builtin
		;;:ensure t
		:config
		;;(setq org-id-link-to-org-use-id t)
		(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

	;; org-sidebar
	(use-package org-sidebar
		:ensure t
		:bind ("C-c s" . org-sidebar-toggle)
		:config
		(setq org-sidebar-side 'left)
		(setq org-sidebar-default-fns '(org-sidebar-tree-view-buffer
																		org-sidebar--todo-items))))

;; window(builtin)
(use-package window
	:config
	(setq display-buffer-alist
				'(
					("\\*Warnings\\*"
					 (display-buffer-reuse-mode-window display-buffer-below-selected))
					("\\*vterm\\*"
					 (display-buffer-reuse-window display-buffer-below-selected)))))

;; winner(builtin)
(use-package winner
	:config (winner-mode 1)
	:bind
	(("C-x p" . 'winner-undo)
	 ("C-x n" . 'winner-redo))
	:config
	(setq winner-dont-bind-my-keys t))

;; window-number
(use-package window-number
	:ensure t
	;; unworked :init (window-number-meta-mode)
	:config (window-number-meta-mode))

;; which-key
(use-package which-key
	:ensure t
	:hook (after-init . which-key-mode))

;; goto-chg
(use-package goto-chg
	:ensure t
	:bind
	(("C-q p" . goto-last-change)
	 ("C-q n" . goto-last-change-reverse)))

;; hl-line-mode
(use-package hl-line
	:disabled
	:ensure t
	:config (global-hl-line-mode))

;; hl-todo
(use-package hl-todo
	:ensure t
	:config
	(setq hl-todo-keyword-faces
				'(("XXX" . "firebrick")
					("TODO" . "firebrick")
					("DONE" . "firebrick")
					("HACK" . "firebrick")
					("NOTE" . "firebrick")
					("FIXME" . "firebrick")))
	(setq hl-todo-highlight-punctuation ":")
	(setq hl-todo-require-punctuation t)
	(global-hl-todo-mode))

;; simple-modeline
(use-package simple-modeline
	:ensure t
	:config 
	(setq simple-modeline-segments
				'((simple-modeline-segment-position
					 simple-modeline-segment-modified
					 simple-modeline-segment-buffer-name
					 simple-modeline-segment-vc)
					(simple-modeline-segment-major-mode
					 simple-modeline-segment-eol
					 simple-modeline-segment-encoding)))
	(simple-modeline-mode))

;; vterm
;; NOTE: need external configuration to .bashrc
(use-package vterm
	:if (eq system-type 'gnu/linux)
	:ensure t
	:hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
	:bind
	(:map vterm-mode-map
				("M-p" . nil)
				("M-n" . nil)
				("C-t" . nil))

	:config
	(setq vterm-max-scrollback 10000)
	(use-package vterm-toggle
		:ensure t
		:bind
		(("C-t" . vterm-toggle)
		 ("C-c C-t" . vterm-toggle-cd))))

;; company
(use-package company
	:ensure t
	:hook (prog-mode . global-company-mode)
	:bind
	(("C-M-i" . company-complete)
	 ;;("TAB" . company-indent-or-complete-common)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-j" . company-complete-selection)
	 ("C-h" . nil))
	
	:config
	(setq company-search-regexp-function #'company-search-flex-regexp)
	(setq company-minimum-prefix-length 1)
	(setq company-idle-delay 0)
	(setq company-tooltip-limit 20)
	(setq company-selection-wrap-around t)
	;; disable icons
	(setq company-format-margin-function nil))

;; mozc.el
(use-package mozc
	:if window-system
	:ensure t
	:config
	(setq default-input-method "japanese-mozc")
	;; unwork if truncate-lines != nil
	;;(setq mozc-candidate-style 'overlay))

	(use-package mozc-cand-posframe
		:ensure t
		:after mozc
		:custom-face
		(mozc-cand-posframe-normal-face ((t (:foreground nil :background nil))))
		(mozc-cand-posframe-focused-face ((t (:inherit link :foreground nil :background nil))))
		:config
		(setq mozc-candidate-style 'posframe)))
	
;; migemo
(use-package migemo
	:if (executable-find "cmigemo")
	:ensure t
	;; supress lsp warnings
	:commands migemo-init
	:hook (after-init . migemo-init)
	:config
	(setq migemo-options '("-q" "--emacs"))
	(setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
	(setq migemo-user-dictionary nil)
	(setq migemo-regex-dictionary nil)
	(setq migemo-coding-system 'utf-8-unix))

;; anzu
(use-package anzu
	:ensure t
	:bind
	(("M-%" . anzu-isearch-query-replace)
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
	:bind ("C-q g" . 'ggtags-mode))

;; dumb-jump
(use-package dumb-jump
	:ensure t
	:bind ("C-q M-." . dumb-jump-go-other-window)
	:config
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
	(setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; projectile
(use-package projectile
	:ensure t
	;; unwork :config (projectile-mode +1)
	:init (projectile-mode +1)
	:bind
	(:map projectile-mode-map
				("C-c p" . projectile-command-map)))

;; treemacs
(use-package treemacs
	:ensure t
	:bind ("C-q t" . treemacs)
	:config
	(setq treemacs-no-png-images t))

;; treemacs-projectile
(use-package treemacs-projectile
	:ensure t
	:after treemacs projectile)

;; flycheck
(use-package flycheck
	:ensure t
	:config
	(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
	(global-flycheck-mode))

; flymake(builtin)
(use-package flymake
	:disabled
	:bind
	(:map flymake-mode-map
				("C-q C-p" . flymake-goto-prev-error)
				("C-q C-n" . flymake-goto-next-error)))

;; lsp-mode
;; NOTE: Resuires language servers individually.
;;			 C/C++: pacman -S clang
;;			 python: install python-lsp-server[all] for each project
;;			 golang: go get golang.org./x/tools/gopls@latest
(use-package lsp-mode
	:ensure t
	;;:commands lsp-deferred
	:hook (prog-mode . lsp-deferred)
	
	:config
	;;(setq lsp-log-io t) ;; for debug
	(setq lsp-keymap-prefix "C-q l")
	(setq lsp-warn-no-matched-clients nil)
	(setq lsp-signature-auto-activate nil)
	(setq lsp-completion-provider :capf)
	
	;; clangd args
	;; set log=verbose for debug
	(setq lsp-clients-clangd-args '("-j=2" "--background-index" "--log=error"))

	;; settings per langs
	(setq lsp-register-custom-settings
	 '(("gopls.experimentalWorkspaceModule" t t)))
	
	;; lsp-ui
	(use-package lsp-ui
		:ensure t
		:commands lsp-ui-mode
		:hook (lsp-mode . lsp-ui-mode)
		:bind
		(:map lsp-ui-mode-map
					([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
					([remap xref-find-references] . lsp-ui-peek-find-references) ; M-?
					("C-q C-u m" . lsp-ui-imenu))
		
		:custom-face
		(lsp-ui-sideline-symbol-info ((t (:background "default"))))
		;; background face of sideline and doc
		(markdown-code-face ((t (:background "grey10"))))

		:config
		(setq lsp-lens-enable t)

		;; lsp-ui-doc
		(setq lsp-ui-doc-enable nil)
		(setq lsp-ui-doc-header t)
		(setq lsp-ui-doc-include-signature t)
		(setq lsp-ui-doc-delay 2)

		;; lsp-ui-sideline
		(setq lsp-ui-sideline-enable t)
		(setq lsp-ui-sideline-show-code-actions t)
		(setq lsp-ui-sideline-show-hover nil)
		(setq lsp-ui-sideline-delay 0.2)
		;;(lsp-ui-sideline-update-mode 'line)
		(setq lsp-ui-sideline-show-diagnostics t)
		(setq lsp-ui-sideline-diagnostic-max-lines 10)
		(setq lsp-ui-sideline-diagnostic-max-line-length 150)

		;; lsp-ui-peek
		(setq lsp-ui-peek-always-show t)))

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
		:bind
		(("C-q f b" . clang-format-buffer)
		 ("C-q f r" . clang-format-region))
		:hook
		(c-mode-common . (lambda ()
											 (add-before-save-hook 'clang-format-buffer)))
		
		:config
		(setq clang-format-style "file")
		(setq clang-format-fallback-style "google")))

;; go-mode
(use-package go-mode
	:ensure t
	:after lsp-mode
	:hook
	(go-mode . (lambda ()
							 (add-before-save-hook
								(lambda ()
									(lsp-organize-imports)
									(lsp-format-buffer))))))

;; rust-mode
(use-package rust-mode
	:ensure t
	:after lsp-mode)

;; csharp-mode
;;(use-package csharp-mode
;;	:ensure nil
;;	:config
;;	(setq-default tab-width 4)
;;	(setq-default c-basic-offset 4)
;;	(setq tab-stop-list (number-sequence 4 120 4)))
	

;; python-mode
(use-package python
	:ensure t
	:after lsp-mode
	:hook
	(python-mode . (lambda ()
									 (add-before-save-hook 'lsp-format-buffer)))
	:config
	(if (executable-find "black")
			(setq lsp-pylsp-plugins-black-enabled t)))



;;
;; theme config
;;
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
					 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))

;;(use-package modus-themes
;;	:ensure t
;;	:config
;;	(load-theme 'modus-vivendi t)
;;	;; disable line highlight
;;	(global-hl-line-mode -1)))

(use-package doom-themes
	:if (eq system-type 'gnu/linux)
	:ensure t
	:config
	(setq doom-themes-enable-bold t)
	(setq doom-themes-treemacs-theme "doom-one")
	;; treemacs integration requires icons
	;;(doom-themes-treemacs-config)
	(doom-themes-org-config)

	(load-theme
	 (if window-system
			 'doom-tokyo-night
		 'doom-badger) t))

;; theme for windows
(use-package alect-themes
	:if (and (eq window-system 'w32) (eq system-type 'windows-nt))
	:ensure t
	:config
	(load-theme 'alect-black t))

	
;;
;; windows customize
;;
(when (eq system-type 'windows-nt)
	;; font
	
	
	;; IME config (use ime custom patch)
  ;;(setq default-input-method "W32-IME")
	;;(setq-default w32-ime-mode-line-state-indicator "[--]")
	;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
	;;(w32-ime-initialize)

	;; python27 pyls
	(defun use-python27 ()
		(interactive)
		(setq lsp-clients-python-command '("c:\\python27\\scripts\\pyls.exe"))))


;;(profiler-report)
;;(profiler-stop)

;;
;; experiments
;;




;;
;; end of origin
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" default))
 '(global-flycheck-mode t)
 '(package-selected-packages
	 '(nhexl-mode org-id org-sidebar migemo dumb-jump yasnippet-snippets window-number which-key wgrep vterm-toggle use-package treemacs-projectile simple-modeline rust-mode powershell mood-line lsp-ui ido-vertical-mode ido-completing-read+ hl-todo hcl-mode goto-chg google-c-style go-mode gnu-elpa-keyring-update ggtags flycheck doom-themes csv-mode csharp-mode company clang-format anzu amx alect-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
