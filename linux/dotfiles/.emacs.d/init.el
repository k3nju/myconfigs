;;;; init.el, from hell
;; TODO: consider using rotate. https://github.com/daichirata/emacs-rotate
;; TODO: consider create personal keymap
;; TODO: use repeat-map
;; TODO: echo area. boxed string resize mode line. try describe-key and C-\ shows undefined

;;(profiler-start 'cpu)


;;; init.el debugging
(when t
	(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
	(setq debug-on-error t))


;;; supress gc
(setq gc-cons-threshold (* 64 1024 1024))


;;; .el related configs
;; prefer newer version .el over .elc
(setq load-prefer-newer t)
;; enable jit compile 
(setq native-comp-jit-compilation t)


;;; package initialization
(when (require 'package nil t)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
	(setq package-install-upgrade-built-in t)
	(setq package-native-compile t)
	(package-initialize)
	(unless (package-installed-p 'use-package)
		(package-refresh-contents)
		(package-install 'use-package)))


;;; use-package configs
(setq use-package-always-defer t)


;;; emacs basics
(use-package emacs
	:init
	;;; workarounds
	(setq byte-compile-warnings '(cl-functions))

	
	;;; UI/UX
	;; TODO: consider using early-init.el	
	;; fullscreen
	(add-to-list 'default-frame-alist '(fullscreen . maximized))
	;; disable bars
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	;; disable cursor blinking
	(blink-cursor-mode 0)
	;; disable startup screen and messages
	(setq inhibit-startup-screen t)
	;;(setq inhibit-startup-echo-area-message (user-login-name)) ; unwork
	(fset 'display-startup-echo-area-message 'ignore)
	;; scratch buffer message
	(setq initial-scratch-message ";; *scratch*\n")
	;; show line numers
	(setq-default display-line-numbers-width 3)
	(global-display-line-numbers-mode)
	;; show line and column number in mode line
	(line-number-mode)
	(column-number-mode)
	;; show indicators on fringe in visual-line-mode
	(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
	;; resize window to fit a contents of temporal buffer for '*Completions*'
	(temp-buffer-resize-mode)

	;; yes or no -> y/n
	(setq use-short-answers t)
	;; use minibuffer to answer
	(setq use-dialog-box nil)
	
	
	;;; coding system
	(set-language-environment 'utf-8)
	(set-language-environment 'utf-8)
	(prefer-coding-system 'utf-8)
	(set-default-coding-systems 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(set-keyboard-coding-system 'utf-8)
	(setq-default buffer-file-coding-system 'utf-8)
	(when (eq system-type 'windows-nt)
		(setq-default default-process-coding-system '(utf-8 . japanese-cp932-dos)))
	(setq default-input-method "japanese") ; needed?

	
	;;; font for linux
	;;(set-frame-font "Office Code Pro 11")
	(when (and (eq window-system 'x) (eq system-type 'gnu/linux))
		(let* (;; create new fontset for customization
					 ;; XXX: following may be better to create new fontset
					 ;;(create-fontset-from-ascii-font
					 ;; (format "%s-%d:weight=normal:slant=normal" "input mono condensed" 15) nil "myfontset")
					 (myfontset (create-fontset-from-fontset-spec
											 (font-xlfd-name
												(font-spec :name "myfontset" :registry "fontset-myfontset"))))
					 ;; mappings for charset and font name
					 (mappings '(;; for alphabets
											 (:charset ascii
												:spec (:name "input mono condensed" :size 12.0))
											 ;; for japanese
											 ;; XXX: should be used 'unicode
											 ;; https://extra-vision.blogspot.com/2016/07/emacs.html
											 (:charset japanese-jisx0213.2004-1
												;;:spec (:name "ipaexgothic" :size 12.0)
												:spec (:name "source han sans jp" :size 9.3)
												;; to supress flicking)
												:rescale 0.999))))
			(mapc
			 (lambda (m)
				 (let* ((charset (plist-get m :charset))
								(fspec (apply #'font-spec (plist-get m :spec)))
								(rescale (plist-get m :rescale)))
					 (when (find-font fspec)
						 (set-fontset-font myfontset charset fspec))
					 (when rescale
						 (add-to-list
							'face-font-rescale-alist
							(cons (format ".*%s.*" (plist-get (plist-get m :spec) :font))
										rescale)))))
			 mappings)
			(add-to-list 'default-frame-alist '(font . "fontset-myfontset"))))
	
	;;; font for windows
	(when (and (eq window-system 'w32) (eq system-type 'windows-nt))
		;; on windows, must specify font names by using CamelCase. not "input mono condensed"
		(let* ((name "InputMonoCondensed"))
			(if (find-font (font-spec :name name))
					(set-frame-font name)
				;; fallback
				(set-frame-font "MS Gothic 12" nil t))))


	;;; editing visibilities/operabilities
	;; truncate lines
	(setq-default truncate-lines t)
	(setq-default truncate-partial-width-windows nil)
	;; tab
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)
	(setq tab-stop-list (number-sequence 2 120 2))
	(setq tab-always-indent 'complete)
	;; disable bell and screen flashing
	(setq ring-bell-function 'ignore)

	;; use C-h as backspace
	(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
		;; stop cursor on per humps for CamelCase
	(global-subword-mode t)
	;; copy & paste
	(setq select-enable-primary t) ; enable x-window primary selection
	(setq select-enable-clipboard t)
	(setq save-interprogram-paste-before-kill t)
	;; typed text replaces the selection
	(delete-selection-mode t)
	;; scroll
	(setq scroll-conservatively 1) ; scroll per lines
	(setq scroll-margin 0) ; lines remaining to start scrolling
	(setq next-screen-context-lines 3) ; paging with leaving lines of previous page
	(setq scroll-preserve-screen-position t) ; keep screen position when paging
	;; search defaults
	(setq case-fold-search t) ; case sensitive
	;; visit abstract path for find-file
	;; https://memo.sugyan.com/entry/20120105/1325766364
	(setq find-file-visit-truename t)
	;; enable auto-revert. reflect changes made by other process
	(global-auto-revert-mode t)
	;; dedupe minibuffer history
	(setq history-delete-duplicates t)
	

	;;; configurations of automatically created files
	;; disable lock file(.#hoge.txt)
	(setq create-lockfiles nil)
	;; backup files(hoge.txt~)
	(setq version-control t)
	(setq kept-new-versions 5)
	(setq kept-old-versions 0)
	(setq delete-old-versions t)
	(setq backup-by-copying t)
	(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
	;; disable auto-save file(#hoge.txt#)
	(setq auto-save-default nil)
	;; auto-saving to visited(actual) files
	(setq auto-save-visited-interval 30) ;; seconds
	(auto-save-visited-mode t)
	
	
	;;; elisp user site
	(let* ((default-directory (expand-file-name "lisp" user-emacs-directory)))
		(add-to-list 'load-path default-directory)
		(when (file-exists-p default-directory)
			(normal-top-level-add-subdirs-to-load-path)))
	
	
	;;; miscs 
	;; https://www.reddit.com/r/emacs/comments/q0kmw3/psa_sentenceenddoublespace/
	(setq-default sentence-end-double-space nil)


	;;; helpers
	(defun my/pp (&rest vals)
		(mapc (lambda (v) (princ (format "###|%s\n" v))) vals)
		nil)
	(defun my/add-before-save-hook (f)
		(add-hook 'before-save-hook f nil 'local))


	;;; basic hooks
	:hook
	(;; disable electric-indent-mode always
	 (after-change-major-mode . (lambda () (electric-indent-mode -1))))


	;;; basic key bindings
	:bind
	(:map global-map
				;; disable quoted-insert
				("C-q" . nil)
				;; disable suspend-frame
				("C-z" . nil)
				;; disable toggle-input-method
				("C-\\" . nil)
				
				;; move cursor to next window
				("M-n" . other-window)
				;; move cursor to previous window
				("M-p" . (lambda () (interactive) (other-window -1)))
				
				;; input method
				("<zenkaku-hankaku>" . (lambda () (interactive) (toggle-input-method)))
				
				;; swap search key bindings
				("C-s" . isearch-forward-regexp)
				("C-r" . isearch-backward-regexp)
				("C-M-s" . isearch-forward)
				("C-M-r" . isearch-backword)
				
				;; switching buffer
				("C-M-p" . previous-buffer)
				("C-M-n" . next-buffer)
				
				;; personals
				("C-q C-x r" . (lambda () (interactive) (load-file "~/.emacs.d/init.el")))))


;;; basic packages

;; hexl(builtin)
(use-package hexl
	:init
	(setq hexl-bits 8))

;; paren(builtin). show pair of parens
(use-package paren
	:demand t
	:init
	;; show paren when a cursor(|) at
	;; (|setq hoge 1)
	(setq show-paren-when-point-inside-paren t)
	;; | (setq ...)
	(setq show-paren-when-point-in-periphery t)
	:config
	(show-paren-mode t))

;; NOTE: noisy. disabled
;; elec-pair(builtin). inserting parens automatically
(use-package elec-pair
	:disabled
	:hook
	((prog-mode org-mode) . electric-pair-mode))

;; dired(builtin). the directory editor
(use-package dired
	:init
	;; suggest a target for moving/copying intelligently
	(setq dired-dwim-target t)
	(setq dired-recursive-copies 'always)
	(setq dired-recursive-deletes 'top) ;; ask
	(setq dired-create-destination-dirs 'always)
	;; XXX: unworking?
	(setq dired-kill-when-opening-new-dired-buffer t))

;; ediff(builtin). emacs differ
(use-package ediff
	:init
	(setq ediff-window-setup-function 'ediff-setup-windows-plain)
	(setq ediff-split-window-function 'split-window-horizontally))

;; uniquify(builtin). unique buffer names
(use-package uniquify
	:demand t
	:init
	(setq uniquify-buffer-name-style 'forward))

;; recentf(builtin). open file history
(use-package recentf
	:demand t
	:init
	(setq recentf-max-saved-items 50)
	(setq recentf-exclude `("recentf"
													"ido.last"
													"bookmarks"
													,(expand-file-name package-user-dir)))
	:hook
	(after-init . recentf-mode))
;;	:config
;;	(recentf-mode t))

;; savehist(builtin). minibuffer history
(use-package savehist
	:demand t
	:config
	(savehist-mode t))

;; saveplace(builtin). save last cursor position
(use-package saveplace
	:demand t
	:config
	(save-place-mode t))

;; which-key(builtin from emacs30). showing keybinding help
;; NOTE: embark will set prefix-help-command to embark-prefix-help-command
(use-package which-key
	:demand t
	:init
	(setq which-key-idle-delay 0.5)
	;; NOTE: no need to set to DEL. just hit F1.
	;;(setq which-key-paging-key (kbd "DEL"))
	(setq which-key-compute-remaps t)
	:config
	(which-key-setup-side-window-right)
	(which-key-mode))

;; window(builtin). window layout management
(use-package window
	:init
	(setq display-buffer-alist
				'(("\\*Warnings\\*"
					 (display-buffer-reuse-mode-window display-buffer-below-selected))
					("\\*Help\\*"
					 (display-buffer-reuse-window display-buffer-in-side-window)
					 (side . left)
					 (window-width . fit-window-to-buffer))
					("\\*vterm\\*"
					 (display-buffer-reuse-window display-buffer-below-selected)))))

;; NOTE: disabled. not used much
;; winner(builtin). window layout displacement undo/redo
(use-package winner
	:disabled
	:bind
	(("C-q w u" . winner-undo)
	 ("C-q w r" . winner-redo))
	:init
	(setq winner-dont-bind-my-keys t)
	:config
	(winner-mode t))

;; window-number. moving the cursor on window by alt-1|2|3 
(use-package window-number
	:ensure t
	:demand t
	:config
	(window-number-meta-mode t))

;; simple-modeline. modeline customization
(use-package simple-modeline
	:ensure t
	:demand t
	:init
	(setq simple-modeline-segments
				'((simple-modeline-segment-position
					 simple-modeline-segment-modified
					 simple-modeline-segment-buffer-name
					 simple-modeline-segment-vc)
					(simple-modeline-segment-major-mode
					 simple-modeline-segment-eol
					 simple-modeline-segment-encoding)))
	:config
	(simple-modeline-mode t))

;; hl-line-mode. highlight cursor line
(use-package hl-line
	:ensure t
	:demand t
	:config
	(global-hl-line-mode t))

;; hl-todo. highlight code tags
(use-package hl-todo
	:ensure t
	:demand t
	:init
	(setq hl-todo-keyword-faces
				'(("NOTE" . "firebrick")
					("SIDENOTE" . "tomato")
					("XXX" . "firebrick")
					("HACK" . "firebrick")
					("TODO" . "firebrick")
					("DONE" . "steelblue")
					("FIXME" . "firebrick")
					("WORKAROUND" . "firebrick")
					("EXPERIMENT" . "firebrick")
					("DECIDED" . "mediumseagreen")
					("UNDECIDED" . "firebrick")))
	(setq hl-todo-highlight-punctuation ":")
	(setq hl-todo-require-punctuation t)
	:config
	(global-hl-todo-mode t))

;; NOTE: disabled. not used much
;; anzu. display current matchs and total matchs
(use-package anzu
	:disabled
	:ensure t
	:demand t
	:bind
	(("M-%" . anzu-isearch-query-replace)
	 ("C-M-%" . anzu-isearch-query-replace-regexp))
	:init
	(setq anzu-search-threshold 999)
	:config
	(global-anzu-mode t))

;; goto-chg. back to where edited in the past 
(use-package goto-chg
	:ensure t
	:bind
	(("C-q c p" . goto-last-change)
	 ("C-q c n" . goto-last-change-reverse)))

;; undo-fu. undo/redo enhancements
(use-package undo-fu
	:ensure t
	:bind
	(("C-/" . undo-fu-only-undo)
	 ;; M-/ orig. dabbrev-expand
	 ("M-/" . undo-fu-only-redo)))

;; wgrep. materializing grep results 
(use-package wgrep
	:ensure t
	:init
	;; in wgrep mode, C-x C-s will save changes to not only buffers but actual files
	(setq wgrep-auto-save-buffer t))

;; vterm. fast terminal
;; NOTE: need external configuration to .bashrc
(use-package vterm
	:if (eq system-type 'gnu/linux)
	:ensure t
	:hook
	(vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
	:init
	;; vterm-toggle. make vterm toggle-able
	(use-package vterm-toggle
		:if (eq system-type 'gnu/linux)
		:ensure t
		:bind
		(("C-t" . vterm-toggle)
		 ("C-c C-t" . vterm-toggle-cd)
		 :map vterm-mode-map
		 ("C-t" . vterm-toggle))))


;;; jp env packages
;; TODO: refine mozc. see https://apribase.net/2024/07/25/emacs-language-environment-linux/

;; mozc.el. japanese input
(use-package mozc
	:disabled
	:if window-system
	:ensure t
	:init
	(setq default-input-method "japanese-mozc")
	;; NOTE: doen't work well when truncate-lines is t.
	;;       use posframe insted of overlaying.
	;;(setq mozc-candidate-style 'overlay))
	:config
	;; mozc-cand-posframe. show japanese candidates in in-buffer
	(use-package mozc-cand-posframe
		:ensure t
		:demand t ; required "demand t" to load mozc-cand-posframe on mozc :config section executed
		:after mozc
		:custom-face
		(mozc-cand-posframe-normal-face ((t (:foreground unspecified :background unspecified))))
		(mozc-cand-posframe-focused-face ((t (:inherit link :foreground unspecified :background unspecified))))
		:init
		(setq mozc-candidate-style 'posframe)))
	
;; migemo. search japanese words in alphabets
(use-package migemo
	:ensure t
	:demand t
	:preface
	(defun my/find-migemo ()
		(let* ((exec-path (cons (expand-file-name "cmigemo-default-win64" user-emacs-directory)
														exec-path)))
			(executable-find "cmigemo")))
	:if (my/find-migemo)
	:init
	(setq migemo-command (my/find-migemo))
	(setq migemo-options '("-q" "--emacs"))
	(setq migemo-dictionary
				(if (eq system-type 'windows-nt)
						(expand-file-name "cmigemo-default-win64/dict/utf-8/migemo-dict" user-emacs-directory)
					"/usr/share/migemo/utf-8/migemo-dict"))
	(setq migemo-user-dictionary nil)
	(setq migemo-regex-dictionary nil)
	(setq migemo-coding-system 'utf-8-unix)
	(setq migemo-use-default-isearch-keybinding nil)
	:config
	(migemo-init))


;;; modern emacs enhancements

;; orderless. filtering completion candidates
;; TODO: fine grained tuning
;;       - orderless-matching-styles
;;       - completion-category-overrides
(use-package orderless
	:ensure t
	:demand t
	:config
	;;(setq orderless-component-separator ",")

	;; default matching styles
	(orderless-define-completion-style my/orderless-default
		(orderless-matching-styles '(orderless-literal
																 orderless-regexp)))
	(setq completion-styles '(my/orderless-default basic))
	(setq completion-category-defaults nil)
	(setq completion-category-overrides nil)

	;; XXX: opening files by tramp(e.g.: /ssh:hoge@hoge:~/) will fail?
	;;(setq completion-category-overrides '((file (styles basic))))

	;; define matching styles for in-buffer completion
	(orderless-define-completion-style my/orderless-in-buffer
	  (orderless-matching-styles '(orderless-literal
																 orderless-regexp
																 orderless-flex)))
	
	;; disable eglot completion and use orderless
	(setq completion-category-overrides '((eglot (styles my/orderless-in-buffer))
																				(eglot-capf (styles my/orderless-in-buffer)))))

;; NOTE: disabled. trying orderless
;; prescient. matching for completion candidates
(use-package prescient
	:disabled
	:ensure t
	:custom-face
	(prescient-primary-highlight
	 ((t :foreground "#b3a3e9" :background "#2a273a" :weight ultra-bold)))
	:init
	;;	(add-to-list 'completion-styles-alist
	;;             '(tab completion-basic-try-completion ignore
	;;               "Completion style which provides TAB completion only."))
	;;	(setq completion-styles '(tab basic))
	;;	(setq completion-styles '(basic))
	;;	;;(setq completion-styles '(prescient basic))
	(setq prescient-sort-full-matches-first t)
	:config
	(prescient-persist-mode)

	(use-package corfu-prescient
		:ensure t
		:after corfu
		:init
		(setq corfu-prescient-enable-filtering t)
		(setq corfu-prescient-enable-sorting t)
		:config
		;; to enable corfu-expand.
		;; HINT: https://github.com/minad/corfu/issues/170
		;; HINT: https://github.com/minad/corfu?tab=readme-ov-file#expanding-to-the-common-candidate-prefix-with-tab
		(add-to-list 'completion-styles-alist
								 '(tab completion-basic-try-completion ignore
											 "Completion style which provides TAB completion only."))
		(setq corfu-prescient-completion-styles '(tab prescient basic))
		(corfu-prescient-mode))

	(use-package vertico-prescient
		:ensure t
		:after vertico
		:init
		(setq vertico-prescient-enable-filtering t)
		(setq vertico-prescient-enable-sorting t)
		:config
		(vertico-prescient-mode)))

;; corfu. in-buffer completion UI
(use-package corfu
	:if window-system
	:ensure t
	:bind
	(:map corfu-map
				;; tab to next entry
				;;("TAB" . corfu-next)
				;;("<tab>" . corfu-next)
				
				;;("C-j" . corfu-complete)
				;;("C-j" . corfu-expand)
				
				;; C-a is bound to corfu-prompt-begging. restore to default.
				;; it used to work with move-beginning-of-line,
				;; but now it has to be beginning-of-visual-line.
				([remap corfu-prompt-beginning] . beginning-of-visual-line)
				("M-SPC" . corfu-insert-separator)
				("C-SPC" . corfu-insert-separator))
	:init
	;;(setq corfu-separator " ") ;; need to be matched to orderless-component-separator
	(setq corfu-cycle t)
	(setq corfu-auto t)
	(setq corfu-count 15)
	(setq corfu-auto-delay 0)
	(setq corfu-auto-prefix 1)
	(setq corfu-preselect 'directory)
	(setq corfu-on-exact-match 'insert)
	(setq corfu-quit-at-boundary 'separator)
	(setq corfu-quit-no-match 'separator)
	
	;; must be in :init
	(global-corfu-mode)
	:config
	(use-package kind-icon
		:ensure t
		:init
		;; disable icon
		(setq kind-icon-use-icons nil)))

;; cape. completions sources
(use-package cape
	:ensure t
	:config
	(setq cape-dabbrev-min-length 6)
	;; same-mode buffers
	(setq cape-dabbrev-check-other-buffers #'cape--buffers-major-mode)
	
	(defun my/cape-defaults ()
		(cape-wrap-super
		 #'cape-dabbrev
		 #'cape-file
		 #'cape-keyword))
	(add-to-list 'completion-at-point-functions #'my/cape-defaults)

	(defun my/cape-inside-string ()
		(cape-wrap-inside-string
		 (cape-capf-super
			#'cape-file
			#'cape-dabbrev)))

	(defun my/cape-inside-comment ()
		(cape-wrap-inside-comment
		 (cape-capf-super
			#'cape-file
			#'cape-dabbrev)))

	(defun my/cape-inside-code ()
		(cape-wrap-inside-code
		 (cape-capf-super
			#'cape-keyword
			#'cape-dabbrev)))

	;; XXX: cape-capf-buster changes corfu previewing
	;;      (cape-capf-buster (cape-capf-super #'cape-dabbrev #'cape-file))))
	;; XXX: cape-wrap-super doesn't work
	;;(defun my/cape-capfs ()
	;;		(cape-wrap-super #'cape-dabbrev #'cape-file))
	;; XXX: combining cape-capf-super and cape-file is buggy
	;; (add-to-list 'completion-at-point-functions
	;;              (cape-capf-super #'cape-file))
	;; typing up to "/usr/bi", then "bin/" appears as a candidate.
	;; but, then select it, "/usr/bi" replaced with only "bin/".
	;; the parent directory "/usr/" is erased.
	;; (cape-capf-super (cape-company-to-capf #'company-files)) works well.
	;; but it's a little bit slow.
	)

;; vertico. minibuffer completion UI
(use-package vertico
	:ensure t
	:demand t
	:init
	(setq vertico-cycle t)
	(setq vertico-count 20)
	(setq vertico-resize nil)
	(setq vertico-preselect 'first)
	;; case insensitive on minibuffer completion
	(setq read-file-name-completion-ignore-case t)
	(setq read-buffer-completion-ignore-case t)
	(setq completion-ignore-case t)
	:config
	(vertico-mode)
	;; vertico-directory is bundled within vertico extensions.
	(use-package vertico-directory
		:ensure nil
		:bind
		(:map vertico-map
					;; disable invoking dired by RET. alt, M-RET invokes dired.
					("RET" . vertico-directory-enter)
					("C-j" . vertico-directory-enter)
					("DEL" . vertico-directory-delete-char)
					("M-DEL" . vertico-directory-delete-word))
		:hook
		(rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; consult. minibuffer commands
(use-package consult
	:ensure t
	:demand t
	:bind (;; C-c bindings in mode-specific-map
				 ("C-c M-x" . consult-mode-command)
				 ;; ("C-c k" . consult-kmacro)
				 ("C-c m" . consult-man)
				 ;; org-mode
				 ;; shortcut for opening org. experiment
				 ("C-c A" . consult-org-agenda)
				 ("C-c H" . consult-org-heading)
				 ;; C-x bindings in ctl-x-map
				 ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
				 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
				 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
				 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
				 ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
				 ;;("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

				 ;; bindings for fast register access
				 ("C-x r s" . consult-register-store) ;; orig. copy-to-register
				 ("C-x r j" . consult-register-load) ;; orig. jump-to-register
				 ("C-x r r" . consult-register) ;; orig. copy-rectangle-to-register
				 
				 ;; yank
				 ("M-y" . consult-yank-pop) ;; orig. yank-pop
				 
				 ;; M-g bindings in goto-map
				 ;;("M-g e" . consult-compile-error)
				 ;;("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
				 ("M-g g" . consult-goto-line) ;; orig. goto-line
				 ("M-g M-g" . consult-goto-line) ;; orig. goto-line
				 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
				 ("M-g m" . consult-mark)
				 ("M-g M" . consult-global-mark)
				 ("M-g i" . consult-imenu)
				 ("M-g I" . consult-imenu-multi)
				 
				 ;; M-s bindings in search-map
				 ("M-s f" . consult-find)
				 ("M-s l" . consult-locate)
				 ("M-s g" . consult-grep)
				 ("M-s G" . consult-git-grep)
				 ("M-s r" . consult-ripgrep)
				 ("C-;"	 . consult-line) ;; experiment
				 ("M-s L" . consult-line-multi) ;; for multiple buffer
				 ;;("M-s k" . consult-keep-lines) ;; actually editing
				 ("M-s n" . consult-focus-lines) ;; narrowing

				 ;; Isearch integration
				 :map isearch-mode-map
				 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
				 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
				 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
				 ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
				 ("M-s x" . consult-isearch-forward)
				 
				 ;; Minibuffer history
				 :map minibuffer-local-map
				 ("M-h" . consult-history))
	:hook
	;; enable preview at point in the *Completion* buffer
	(completion-list-mode . consult-preview-at-point-mode)
	:init
	(setq register-preview-delay 0.0)
	(setq register-preview-function #'consult-register-format)
	(advice-add #'register-preview :override #'consult-register-window)
	;; Use Consult to select xref locations with preview
	(setq xref-show-xrefs-function #'consult-xref)
	(setq xref-show-definitions-function #'consult-xref)
	:config
	(consult-customize
	 ;; applying emacs theme
	 consult-theme
	 :preview-key '(:debounce 0.2 any)
	 
	 ;; preview by M-.
	 consult-ripgrep consult-git-grep consult-grep
	 consult-bookmark consult-recent-file consult-xref
	 consult--source-bookmark consult--source-file-register
	 consult--source-recent-file consult--source-project-recent-file
	 :preview-key "M-.")
	;; narrowing on consult-buffer
	;; Optionally configure the narrowing key.
	;; Both < and C-+ work reasonably well.
	(setq consult-narrow-key "<"))

;; marginalia. enrichment minibuffer annotations
(use-package marginalia
	:ensure t
	:demand t
	:bind
	(:map minibuffer-local-map
				("M-A" . marginalia-cycle))
	:init
	;; must be :init from official readme
	(marginalia-mode t))

;; embark. right-click context menu for emacs
(use-package embark
	:ensure t
	:bind
	(("C-." . embark-act)
	 ("C-," . embark-dwim)
	 ("C-q C-e C-b" . embark-bindings))
	:init
	(setq prefix-help-command #'embark-prefix-help-command)
	:config
	;; Hide the mode line of the Embark live/completions buffers
	(add-to-list 'display-buffer-alist
							 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
	(use-package embark-consult
		:ensure t
		:hook
		(embark-collect-mode . consult-preview-at-point-mode)))


;;; the org-mode
;; org
(use-package org
	:ensure t
	:demand t
	:bind
	(("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 :map org-mode-map
	 ("M-," . org-mark-ring-goto)
	 ;; on terminal, "C-," is recognized to ",". on GUI works well.
	 ;; change "C-c ,"(org-priority) to org-insert-structure-template and
	 ;; rebind "C-c p" to org-priority
	 ("C-c ," . org-insert-structure-template)
	 ("C-c p" . org-priority))
	:custom-face
	;; :extend uneffected?
	(org-level-1 ((t (:extend t :underline t :weight ultra-bold :height 1.5))))
	(org-level-2 ((t (:extend t :weight bold :height 1.3))))
	(org-level-3 ((t (:weight bold :height 1.1))))
	:hook
	(org-mode . hl-todo-mode) ;; global-hl-todo-mode is not effective on org-mode
	(org-mode . (lambda () (setq-local completion-at-point-functions (list #'my/cape-defaults))))
	(org-capture-after-finalize . (lambda ()
																	;; HACK: if misc template invoked and that is aborted, delete a note file
																	(let* ((is-misc (org-capture-get :misc-note))
																				 (note-file-name (buffer-file-name (org-capture-get :buffer))))
																		(when (and is-misc
																							 org-note-abort
																							 (file-exists-p note-file-name))
																			(delete-file note-file-name)))))
	:config
	;; org-directory precedence
	;; 1) ~/Dropbox/org/
	;; 2) ~/.emacs.d/org/
	;; 3) ~/org/ (org-directory default)
	(let* ((ds (if (eq system-type 'windows-nt)
								 ;; org/ candidates for windows
								 (let* ((win-home-dir (expand-file-name (getenv "HOMEPATH") (getenv "HOMEDRIVE"))))
									 (list (expand-file-name "Dropbox" win-home-dir)
												 user-emacs-directory
												 win-home-dir))
							 ;; org/ candidates for linux
							 (list "~/Dropbox/"
										 user-emacs-directory
										 "~/"))))
		(mapc (lambda (d)
						(when (file-exists-p d)
							(setq org-directory d)))
					(mapcar (apply-partially 'expand-file-name "org/") ds)))

	(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
	(setq my/misc-notes-directory (expand-file-name "misc" org-directory))
	(mkdir my/misc-notes-directory t)
	(setq my/blog-pages-directory (expand-file-name "blog" org-directory))
	(mkdir my/blog-pages-directory t)

	;; org capture
	(defun my/get-note-name (dir)
		(interactive)
		(let* ((get-filename (lambda ()
													 (expand-file-name
														(format "%s_%s.org"
																		(format-time-string "%Y-%m-%d")
																		(read-string "Note name: "))
														dir)))
					 (filename (funcall get-filename)))
			(while (file-exists-p filename)
				(setq filename (funcall get-filename)))
			filename))
	
	(setq org-capture-templates
				'(("n" "[N]otes" entry (file+headline "notes.org" "Notes")
					 "* %T %? \n" 
					 :empty-lines 0 :kill-buffer 1 :prepend t)
					("j" "[J]ournals" entry (file+headline "notes.org" "Journals")
					 "* %T %? \n"
					 :empty-lines 0 :kill-buffer t :prepend t)
					("d" "[D]iary" entry (file "diary.org")
					 "* %T\n%? \n"
					 :empty-lines-after 1 :prepend t :jump-to-captured t)
					;; NOTE: plain cant refile to other org files
					("m" "[M]isc notes" plain (file (lambda () (my/get-note-name my/misc-notes-directory)))
					 "* %T %? :misc:\n"
					 :misc-note t :empty-lines-after 2 :jump-to-captured t)
					("b" "[B]log pages" plain (file (lambda () (my/get-note-name my/blog-pages-directory)))
					 "#+title: %? \n#+date: %T\n\n* {{{date}}} | {{{title}}}\n"
					 :misc-note t :empty-lines-after 2 :jump-to-captured t)))
	
	;; agenda config
	(setq org-agenda-files (list org-directory my/misc-notes-directory))
	(setq org-agenda-custom-commands
				'(;; notes relateds
					("n" . "Notes/Journals")
					("nn" "[N]otes in default notes file"
					 tags "+n"
					 ((org-agenda-files (list org-default-notes-file))))
					("nj" "[J]ournals in default notes file"
					 tags "+j"
					 ((org-agenda-files (list org-default-notes-file))))
					("nm" "[M]isc notes"
					 tags "+misc"
					 ((org-agenda-files (list my/misc-notes-directory))))

					;; eisenhower's matrix
					("e" "[E]isenhower matrix"
					 ((tags-todo "+priority=1"
											 ((org-agenda-overriding-header "Do It | Important & Urgent")
												(org-agenda-todo-keyword-format "")))
						(tags-todo "+priority=2"
											 ((org-agenda-overriding-header "Schedule | Important & Non-urgent")
												(org-agenda-block-separator ?-)
												(org-agenda-todo-keyword-format "")))
						(tags-todo "+priority=3"
											 ((org-agenda-overriding-header "Delegate | Unimportant & Urgent")
												(org-agenda-block-separator ?-)
												(org-agenda-todo-keyword-format "")))
						(tags-todo "+priority=4"
											 ((org-agenda-overriding-header "Eliminate | Unimportant & Non-urgent")
												(org-agenda-block-separator ?-)
												(org-agenda-todo-keyword-format "")))
						(tags-todo "*"
											 ((org-agenda-overriding-header "Unprioritized")
												(org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\[#[1-4]\\]"))
												(org-agenda-block-separator ?-)
												(org-agenda-todo-keyword-format "")))))
					))

	;; refiles
	(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
	(setq org-refile-use-outline-path 'full-file-path)
	(setq org-refile-allow-creating-parent-nodes 'confirm)
	(setq org-outline-path-complete-in-steps nil)
	(setq org-archive-location (format "%s/%%s_archive::" (expand-file-name "archive" org-directory)))

	;; org tags
	(setq org-tags-column 0)

	;; org todo
	(setq org-todo-keywords
				'((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
	(setq org-log-done 'time)
	(setq org-enforce-todo-dependencies t)
	(setq org-enforce-todo-checkbox-dependencies t)

	;; org priority
	(setq org-highest-priority 1)
	(setq org-lowest-priority 4)
	(setq org-default-priority 1)

	;; view config
	(setq org-startup-folded nil)
	(setq org-startup-indented t)

	;; miscs
	(setq org-indent-indentation-per-level 1)
	;;(setq org-hide-leading-stars t)
	;;(setq org-adapt-indentation t) ;; hard indentation
	(setq org-return-follows-link t)
	(setq org-hide-emphasis-markers t)
	(setq org-blank-before-new-entry
				'((heading . always)
					(plain-list-item . nil)))
	(setq org-cycle-separator-lines 1)

	(setq org-src-tab-acts-natively t)
	(setq org-src-preserve-indentation t)
	
	;; enable underline to EOL on headings
	(setq org-fontify-whole-heading-line t)

	;; timestamp
	(setq-default org-display-custom-times t)
	(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M:%S>"))

	;; org-id(builtin)
	(use-package org-id
		:demand t
		:config
		;;(setq org-id-link-to-org-use-id t)
		(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))


;;; development packages

;; ggtags. gnu global
(use-package ggtags
	:ensure t
	:bind
	("C-q C-g" . 'ggtags-mode))

;; dumb-jump. ensuring navigating codes work
;; NOTE: dumb-jump is registered as a xref implementation.
;;			 when lsp is not activated, M-. will use dumb-jump via xref interface.
(use-package dumb-jump
	:ensure t
	:init
	(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
	;; replace xref I/F.
	;; e.g.: M-.|C-M-.|M-?
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; project(builtin). project management ops
(use-package project
	:config
	(setq project-vc-extra-root-markers '(".projectile"
																				".project"
																				"requirements.txt"
																				"go.mod")))

;; NOTE: currently, using project.el
;; projectile. project management
;; still usefull even though project.el is in emacs
(use-package projectile
	:disabled
	:ensure t
	;; unwork :config (projectile-mode +1)
	:init (projectile-mode +1)
	:bind-keymap
	("C-x p" . projectile-command-map))

;; NOTE: disabled. tuning required for eglot
;; flymake(builtin). flymake can use eglot as a backend.
(use-package flymake
	:disabled
	:bind
	(:map flymake-mode-map
				("C-q C-p" . flymake-goto-prev-error)
				("C-q C-n" . flymake-goto-next-error))
	:hook
	(prog-mode . flymake-mode)
	:config
	(use-package flymake-diagnostic-at-point
		:ensure t
		:after flymake
		:hook
		(flymake-mode . flymake-diagnostic-at-point-mode)))

;; NOTE: disabled. tuning required for eglot
;; flycheck
(use-package flycheck
	:disabled
	:ensure t
	:init
	(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
	:config
	(global-flycheck-mode))

;; NOTE: disabled. trying tempel
;; yasnippet. snippet provider
(use-package yasnippet
	:disabled
	:ensure t
	:config
	;; actual snippets
	(use-package yasnippet-snippets
		:ensure t
		:init
		(setq yas-prompt-functions '(yas-ido-prompt))
		:config
		(yas-global-mode t)
		(yas-reload-all)))

;; eglot(builtin). 
(use-package eglot
	:disabled
	:after cape
	:hook
	(eglot-managed-mode . my/init-eglot)
	:config
	(setq eglot-ignored-server-capabilities '(:hoverProvider
																						:inlayHintProvider))
	(add-to-list 'eglot-stay-out-of 'flymake)
	(add-to-list 'eglot-stay-out-of 'eldoc)
	
	;; NOTE: it's recommended that wrap eglot-completion-at-point by cape-wrap-buster
	;;       https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
	;;       but, to use orderless-flex on in-buffer completion on corf,
	;;       cape-wrap-buster must be disabled.
	;;       currently, disabling cape-wrap-buster and using orderless-flex filterling.
	;;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
	
	
	(defun my/eglot-cape-inside-code ()
		(cape-wrap-nonexclusive
		 (cape-capf-inside-code
			(cape-capf-super
			 #'eglot-completion-at-point
			 #'cape-dabbrev))))
	
	(defun my/init-eglot ()
		(my/pp "eglot my init")
		(setq-local completion-at-point-functions
								(list
								 #'my/eglot-cape-inside-code
								 #'my/cape-inside-string
								 #'my/cape-inside-comment))))

;; NOTE: disabled experimentally. trying eglot.
;; lsp-mode
;; NOTE: Resuires language servers individually.
;;			 C/C++: pacman -S clang
;;			 python: install python-lsp-server[all] for each project
;;			 golang: go get golang.org./x/tools/gopls@latest
(use-package lsp-mode
	:disabled
	:ensure t
	:hook
	(prog-mode . lsp-deferred)
	:init
	(setq lsp-keymap-prefix "C-q C-l")
	:config
	;;(setq lsp-log-io t) ;; for debug
	(setq lsp-warn-no-matched-clients nil)
	(setq lsp-signature-auto-activate nil)
	;;(setq lsp-completion-provider :capf) ;; for company mode
	(setq lsp-completion-provider :none)
	
	;; clangd args
	;; set log=verbose for debug
	(setq lsp-clients-clangd-args '("-j=2" "--background-index" "--log=error"))
	
	;; settings per langs
	(setq lsp-register-custom-settings
				'(("gopls.experimentalWorkspaceModule" t t)))
	
	;; lsp-ui
	(use-package lsp-ui
		:disabled
		:ensure t
		:after lsp-mode
		:commands lsp-ui-mode
		:hook
		(lsp-mode															. lsp-ui-mode)
		:bind
		(:map lsp-ui-mode-map
					;; remap xref-find-defenitions function to lsp-ui-peek-find-definitions
					([remap xref-find-definitions]	. lsp-ui-peek-find-definitions) ;; M-.
					([remap xref-find-references]		. lsp-ui-peek-find-references) ;; M-?
					("C-q C-u C-m"									. lsp-ui-imenu))
		
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
		(setq lsp-ui-sideline-delay 0		.	2)
		;;(lsp-ui-sideline-update-mode 'line)
		(setq lsp-ui-sideline-show-diagnostics t)
		(setq lsp-ui-sideline-diagnostic-max-lines 10)
		(setq lsp-ui-sideline-diagnostic-max-line-length 150)
		
		;; lsp-ui-peek
		(setq lsp-ui-peek-always-show t)))

;; treesit-auto. tree-sitter lang bundles manager
(use-package treesit-auto
	:if (executable-find "git")
	:ensure t
	:init
	(setq treesit-auto-install 'prompt)
	:config
	(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))

;; experiment
;; paredit. structual editing for lisp
(use-package paredit
	:ensure t
	:hook
	((lisp-data-mode . enable-paredit-mode)))

;; experiment
;; popper. show buffers in popup
(use-package popper
	:ensure t
	:demand t
	:bind
	(("C-q C-p C-p" . popper-toggle)
	 ("C-q C-p C-n" . popper-cycle))
	:init
  (setq popper-reference-buffers
				'("\\*Messages\\*"
					"\\*scratch*\\*"
					"^\\*.*vterm.*\\*$" vterm-mode))
	(setq popper-display-control nil)
	:config
	(popper-mode)
	(popper-echo-mode))


;;; programming language modes

;; elisp-mode(builtin) 
(use-package elisp-mode
	:config
	;; NOTE: disabled. too agressive
	(use-package aggressive-indent
		:disabled
		:ensure t
		:hook
		(emacs-lisp-mode . aggressive-indent-mode)))

;; cc-mode(builtin)
(use-package cc-mode
	;;:after (:and (:any lsp-mode eglot) cape)
	:hook
	(c-mode-common . eglot-ensure)
	:config
	;; google-c-style
	(use-package google-c-style
		:ensure t
		:hook
		(c-mode-common . google-set-c-style))
	;; clang-format
	(use-package clang-format
		:ensure t
		:bind
		(("C-q C-f C-b" . clang-format-buffer)
		 ("C-q C-f C-r" . clang-format-region))
		:hook
		(c-mode-common . (lambda ()
											 (my/add-before-save-hook 'clang-format-buffer)))
		:init
		(setq clang-format-style "file")
		(setq clang-format-fallback-style "google")))

;; go-mode
(use-package go-mode
	:ensure t
	:hook
	;;(go-mode . (lambda ()
	;;						 (my/add-before-save-hook
	;;							(lambda ()
	;;											 (lsp-organize-imports)
	;;											 (lsp-format-buffer))))
	(go-mode . eglot-ensure))

;; python-mode
(use-package python
	:ensure t
	:hook
	;;(python-mode . (lambda ()
	;;								 (my/add-before-save-hook 'lsp-format-buffer)
	;;								 (if (executable-find "black")
	;;										 (setq lsp-pylsp-plugins-black-enabled t))))
	(python-mode . eglot-ensure)
	:init
	(setq-default python-indent-offset 4))

;; rust-mode
(use-package rust-mode
	:ensure t
	:hook
	(rust-mode . eglot-ensure))

;; powershell
(use-package powershell
	:ensure t
	:hook
	(powershell-mode . eglot-ensure)
	:init
	(setq powershell-indent 2))


;;; traditional packages, still usefull.

;; NOTE: disabled. currently using vertico.
;; NOTE: emacs 28 introduced fido-vertical-mode.
;;       consider using this
;; ido(builtin). minibuffer completion UI
(use-package ido
	:disabled
	:init
	(message "ido init")
	:config
	(message "ido config")
	(ido-mode t)
	(setq ido-everywhere t)
	(setq ido-enable-flex-matching t)
	(setq ido-use-virtual-buffers t)
	(setq ido-use-filename-at-point 'guess)
	(setq ido-enable-regexp t)

	;; ido-vertical-mode
	(use-package ido-vertical-mode
		:ensure t
		:init
		(message "ido-vertical init")
		
		:config
		(message "ido-vertical config")
		(ido-vertical-mode t)
		(setq ido-vertical-show-count t)
		(setq ido-vertical-define-keys 'C-n-and-C-p-only))
	
	;; ido-completing-read+
	(use-package ido-completing-read+
		:ensure t
		:config
		(ido-ubiquitous-mode t))

	;; amx
	(use-package amx
		:ensure t
		:bind ("M-x" . amx)
		:config	(amx-mode)
		;; unworked :init (amx-backend 'ido) and :config (amx-backend 'ido)
		:custom	(amx-backend 'ido)))

;; company. traditional in-buffer completion UI
(use-package company
	:if (not window-system)
	:ensure t
	:hook
	(prog-mode . global-company-mode)
	:bind
	(("C-M-i" . company-complete)
	 ;;("TAB" . company-indent-or-complete-common)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-j" . company-complete-selection)
	 ("C-h" . nil))
	:init
	(setq company-backends
				;; TODO: rethink backends order
				'(company-capf
					company-files
					company-clang
					(company-dabbrev-code
					 company-gtags
					 company-keywords)))
	
	;;(setq company-search-regexp-function #'company-search-flex-regexp)
	(setq company-minimum-prefix-length 1)
	(setq company-idle-delay 0)
	(setq company-tooltip-limit 20)
	(setq company-selection-wrap-around t)
	;; disable icons
	(setq company-format-margin-function nil)

	;; NOTE: company-dabbrev is disabled.
	;; company-dabbrev is useful, but difficult to use in JP env
	;;(setq company-dabbrev-downcase nil)
	;;(setq company-dabbrev-ignore-case nil)
	:config
	;; make lsp-mode use company
	(setq lsp-completion-provider :capf))


;;; themes

;; theme for linux
(use-package doom-themes
	:if (eq system-type 'gnu/linux)
	:ensure t
	:demand t
	:init
	(setq doom-themes-enable-bold t)
	;; NOTE: currently, treemacs is unused
	;;(setq doom-themes-treemacs-theme "doom-one")
	;; treemacs integration requires icons
	;;(doom-themes-treemacs-config)
	:config
	(doom-themes-org-config)
	(load-theme
	 (if window-system
			 'doom-tokyo-night
		 'doom-badger)
	 t))

;; theme for windows
;; alt alect-themes
(use-package ef-themes
	:if (and (eq window-system 'w32) (eq system-type 'windows-nt))
	:ensure t
	:demand t
	:config
	(load-theme 'ef-tritanopia-dark t))


;;; load customizations
(mapc
 #'load
 (sort 
	(directory-files
	 ;; ~/.emacs.d/lisp/
	 (expand-file-name "lisp" user-emacs-directory) ;
	 ;; fullname
	 t
	 ;; regex. e.g.: 10-hoge.el
	 "[[:digit:]]-.*\.el"
	 ;; nosort
	 t)
	;; numeric sort  
	(lambda (l r)
		(< (string-to-number (file-name-nondirectory l))
			 (string-to-number (file-name-nondirectory r))))))


;;(profiler-report)
;;(profiler-stop)

;;; EOF

