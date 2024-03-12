;; TODO: consider using embark

;;(profiler-start 'cpu)

(defun my/pp (o) (mapc 'princ (list "###|" o "\n")))


;;
;; basic config
;;

(use-package emacs
	:config
	;; TODO: consider using early-init.el
	
	;; workaround
	(setq byte-compile-warnings '(cl-functions))
	
	;; coding system
	(prefer-coding-system 'utf-8)
	(set-default-coding-systems 'utf-8)
	(set-terminal-coding-system 'utf-8)
	(set-keyboard-coding-system 'utf-8)
	(setq-default buffer-file-coding-system 'utf-8)
	
	;; frame appearances
	(setq inhibit-startup-screen t)
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)

	;; font for linux
	;;(set-frame-font "Office Code Pro 11")
	(when (and (eq window-system 'x) (eq system-type 'gnu/linux))
		(let* (;; custom fontset
					 ;; which should I use?
					 ;;(_myfontset (create-fontset-from-ascii-font
					 ;;		(format "%s-%d:weight=normal:slant=normal" "input mono condensed" 15) nil "myfontset"))
					 (myfontset (create-fontset-from-fontset-spec
											 (font-xlfd-name
												(font-spec
												 :name "myfontset"
												 :weight 'normal
												 :slant 'normal
												 :width 'condensed
												 ;; use floating point for point size
												 :size 13.0 
												 :spacing 'M
												 :registry "fontset-myfontset"))))

					 ;; font and charset mappings applying to myfontset
					 (mappings '((:font "input mono condensed"
												:charset ascii)
											 (:font "ipaexgothic"
												:charset japanese-jisx0213.2004-1
												;; to supress flicking
												:rescale 0.999))))

			;; create fontset per mappings
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
	
	;; font for windows
	(when (and (eq window-system 'w32) (eq system-type 'windows-nt))
		;; on windows, specify font names by using CamelCase. not "input mono condensed", 
		;;(set-frame-font "InputMonoCondensed")
		;; WORKAROUND: input isn't installed
		(set-frame-font "MS Gothic 12" nil t))

	;; disable bell & screen flashes
	(setq ring-bell-function 'ignore)

	;; line and column numbering
	(setq-default display-line-numbers-width 3)
	(global-display-line-numbers-mode)
	;; show line and column number in mode line
	(line-number-mode)
	(column-number-mode)
	
	;; cursor
	(blink-cursor-mode 0)
	(global-subword-mode t) ;; stop cursor on per humps for CamelCase

	;; editing visibilities
	(show-paren-mode t)
	(setq show-trailing-whitespace t)
	(setq-default truncate-lines t)
	(setq-default truncate-partial-width-windows nil)

	;; tab
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)
	(setq tab-stop-list (number-sequence 2 120 2))
	(setq tab-always-indent 'complete)

	;; indentation
	;; disable electric-indent-mode always
	(add-hook 'after-change-major-mode-hook (lambda () (electric-indent-mode -1)))

	;; copy & paste
	(setq select-enable-primary t)
	(setq select-enable-clipboard t)
	(setq save-interprogram-paste-before-kill t)
	
	;; search defaults
	(setq case-fold-search t) ;; case sensitive

	;; visit abs path for find-file
	;; https://memo.sugyan.com/entry/20120105/1325766364
	(setq find-file-visit-truename t)

	;; backup
	(setq version-control t)
	(setq kept-new-versions 5)
	(setq kept-old-versions 0)
	(setq delete-old-versions t)
	(setq backup-by-copying t)
	(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))

	;; for resizing *Completions* buffer size
	;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Temporary-Displays.html#index-temp_002dbuffer_002dresize_002dmode
	(temp-buffer-resize-mode)
	
	;; elisp user site
	(let* ((default-directory (expand-file-name "lisp" user-emacs-directory)))
		(add-to-list 'load-path default-directory)
		(when (file-exists-p default-directory)
			(normal-top-level-add-subdirs-to-load-path))
		;; separate autosaved customizations 
		(setq custom-file (expand-file-name "autosaved-custom.el" default-directory)))
	
	;; package archives
	(when (require 'package nil t)
		(package-initialize)
		(setq package-archives
					'(("gnu" . "https://elpa.gnu.org/packages/")
						("melpa" . "https://melpa.org/packages/")
						("org" . "https://orgmode.org/elpa/"))))
	
	;; install use-package if emacs < 29
	(when (< emacs-major-version 29)
		(unless (package-installed-p 'use-package)
			(message "use-package is not installed and installing it")
			(package-refresh-contents)
			(package-install 'use-package)))
	
	;; ediff
	(setq-default ediff-split-window-function 'split-window-horizontally)
	
	;; yes or no
	(fset 'yes-or-no-p 'y-or-n-p)
	
	;; disable ime on minibuffer
	(add-hook 'minibuffer-setup-hook 'deactivate-input-method)

	;; helpers
	(defun my/add-before-save-hook (f) (add-hook 'before-save-hook f nil 'local))

	;;;; key bindings
	(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
	(setq default-input-method "japanese")
	:bind
	(:map global-map
				;; quoted-insert
				("C-q" . nil)
				;; suspend-frame
				("C-z" . nil)
				;; window move forard
				("M-n" . other-window)
				;; window move backward
				("M-p" . (lambda () (interactive) (other-window -1)))
				;; toggle-input-method
				("C-\\" . nil)
				;; NOTE: disabled. currently using postframe style.
				;; to display candidates for overlay by mozc, truncate-lines must be enabled.
				;;(toggle-truncate-lines) 
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
				("C-q C-x r" . (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
				)
	;; end of config
	)


;;
;; basic packages
;;

;; hexl(builtint)
(use-package hexl
	:init
	(setq hexl-bits 8))

;; uniquify(builtin). unique buffer names
(use-package uniquify
	:init
	(setq uniquify-buffer-name-style 'forward))

;; recentf(builtin). open file history
(use-package recentf
	:init
	(setq recentf-max-saved-items 100)
	(setq recentf-exclude `("recentf"
													"ido.last"
													,(expand-file-name package-user-dir)))
	:config
	(recentf-mode t))

;; savehist(builtin). minibuffer history
(use-package savehist
	:config
	(savehist-mode))

;; window(builtin). window layout management
(use-package window
	:init
	(setq display-buffer-alist
				'(
					("\\*Warnings\\*"
					 (display-buffer-reuse-mode-window display-buffer-below-selected))
					("\\*vterm\\*"
					 (display-buffer-reuse-window display-buffer-below-selected)))))

;; winner(builtin). window layout displacement undo/redo
(use-package winner
	:bind
	(("C-q C-w p" . 'winner-undo)
	 ("C-q C-w n" . 'winner-redo))
	:init
	(setq winner-dont-bind-my-keys t)
	:config
	(winner-mode 1))

;; anzu. display current match and total matchs
(use-package anzu
	:ensure t
	:bind
	(("M-%" . anzu-isearch-query-replace)
	 ("C-M-%" . anzu-isearch-query-replace-regexp))
	:init
	(setq anzu-search-threshold 999)
	:config
	(global-anzu-mode +1))

;; window-number. moving cursor by alt-1|2|3 
;; NOTE: M-1, M-2, M-3
(use-package window-number
	:ensure t
	:config
	(window-number-meta-mode))

;; which-key. showing keybinding in minibuffer
(use-package which-key
	:ensure t
	:config
	(which-key-setup-side-window-right)
	(which-key-mode))

;; goto-chg. cursor history
(use-package goto-chg
	:ensure t
	:bind
	(("C-q p" . goto-last-change)
	 ("C-q n" . goto-last-change-reverse)))

;; hl-line-mode. highlighting cursor line
(use-package hl-line
	:ensure t
	:config
	(global-hl-line-mode))

;; hl-todo. highlighting code tags
(use-package hl-todo
	:ensure t
	:init
	(setq hl-todo-keyword-faces
				'(("NOTE" . "firebrick")
					("XXX" . "firebrick")
					("HACK" . "firebrick")
					("TODO" . "firebrick")
					("DONE" . "firebrick")
					("FIXME" . "firebrick")
					("WORKAROUND" . "firebrick")
					("DECIDED" . "#448a2a")
					("UNDECIDED" . "firebrick")))
	(setq hl-todo-highlight-punctuation ":")
	(setq hl-todo-require-punctuation t)
	:config
	(global-hl-todo-mode))

;; simple-modeline. modeline customization
(use-package simple-modeline
	:ensure t
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
	(simple-modeline-mode))

;; wgrep. materializing grep results 
(use-package wgrep
	:ensure t)

;; neotree
(use-package neotree
	:ensure t
	:bind ("C-x d" . neotree-toggle)
	:init
	(setq neo-theme 'ascii))

;; vterm. fast terminal
;; NOTE: need external configuration to .bashrc
(use-package vterm
	:if (eq system-type 'gnu/linux)
	:ensure t
	:hook
	(vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
	:config
	;; vterm-toggle
	(use-package vterm-toggle
		:ensure t
		:requires vterm
		:bind
		(("C-t" . vterm-toggle)
		 ("C-c C-t" . vterm-toggle-cd)
		 :map vterm-mode-map
		 ("C-t" . vterm-toggle))))

;; org
(use-package org
	:ensure t
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
	;; global-hl-todo-mode is not effective
	((org-mode . hl-todo-mode)
	 (org-capture-after-finalize . (lambda ()
																	 ;; HACK: if misc template invoked and that is aborted, delete a note file
																	 (let* ((is-misc (org-capture-get :misc-note))
																					(note-file-name (buffer-file-name (org-capture-get :buffer))))
																		 (when (and is-misc
																								org-note-abort
																								(file-exists-p note-file-name))
																			 (delete-file note-file-name))))))
	:config
	;; must be set before org-mode loaded
	(setq org-display-custom-times t)
	
	;; org-directory precedence
	;; 1) ~/Dropbox/org/
	;; 2) ~/.emacs.d/org/
	;; 3) ~/org/ (org-directory default)
	(mapc (lambda (v)
					(let* ((d (expand-file-name "org/" v)))
						(when (file-exists-p d)
							(setq org-directory d))))
				;; priority in reverse order
				(list "~/" user-emacs-directory "~/Dropbox/"))
	(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
	(setq misc-notes-directory (expand-file-name "misc" org-directory))

	;; agenda config
	(setq org-agenda-files (list org-directory misc-notes-directory))
	(setq org-agenda-custom-commands
				'(;; notes relateds
					("n" . "[N]otes")
					("nj" "[J]ournals"
					 tags "+journal&+LEVEL=2&+TIMESTAMP>=\"<-1m>\""
					 ;; limit the files to be searched to org-default-notes-file
					 ((org-agenda-files (list org-default-notes-file))))
					("nm" "[M]isc notes"
					 tags "+misc"
					 ((org-agenda-files (list misc-notes-directory))))

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

	;; org todo
	(setq org-todo-keywords
				'((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
	(setq org-log-done 'time)

	;; org priority
	(setq org-highest-priority 1)
	(setq org-lowest-priority 4)
	(setq org-default-priority 1)

	;; view config
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

	(setq org-src-tab-acts-natively t)
	(setq org-src-preserve-indentation t)
	
	;; enable underline to EOL on headings
	(setq org-fontify-whole-heading-line t)

	;; timestamp
	;; to enable time stamp overlay, set org-display-custom-times is t in :init
	(setq org-time-stamp-custom-formats '("<%Y/%m/%d>" . "<%Y/%m/%d %H:%M:%S>"))

	;; org capture
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
				'(("n" "[N]otes" entry (file "notes.org") "* %?\n%T\n" :empty-lines 1 :kill-buffer 1)
					;; NOTE: plain cant refile to other org files
					("m" "[M]isc notes" plain (file create-misc-note-file) "* %?	:misc:\n%T\n"
					 :misc-note t :empty-lines 1 :kill-buffer t)
					("j" "[J]ournals" entry (file+headline "notes.org" "journals") "* %T %?\n" :empty-lines 1 :kill-buffer t :prepend t)
					("d" "[D]iary" entry (file "diary.org") "* %T\n%?\n" :empty-lines 1 :kill-buffer t :prepend t )))

	;; org-id(builtin)
	(use-package org-id
		:requires org
		:config
		;;(setq org-id-link-to-org-use-id t)
		(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

	;; org-sidebar
	(use-package org-sidebar
		:ensure t
		:requires org
		:bind ("C-c s" . org-sidebar-toggle)
		:config
		(setq org-sidebar-side 'left)
		(setq org-sidebar-default-fns '(org-sidebar-tree-view-buffer
																		org-sidebar--todo-items))))


;;
;; jp env packages
;;

;; mozc.el. japanese input 
(use-package mozc
	:if window-system
	:ensure t
	:init
	(setq default-input-method "japanese-mozc")
	;; unwork if truncate-lines != nil
	;;(setq mozc-candidate-style 'overlay))
	)

;; mozc-cand-posframe. show japanese candidates in in-buffer
(use-package mozc-cand-posframe
	:ensure t
	:requires mozc
	:custom-face
	(mozc-cand-posframe-normal-face ((t (:foreground unspecified :background unspecified))))
	(mozc-cand-posframe-focused-face ((t (:inherit link :foreground unspecified :background unspecified))))
	:init
	(setq mozc-candidate-style 'posframe))
	
;; migemo. search japanese words in alphabets
(use-package migemo
	:if (let* ((exec-path (cons (expand-file-name "cmigemo-default-win64" user-emacs-directory) exec-path)))
				(executable-find "cmigemo"))
	:ensure t
	;; to supress lsp warnings
	:commands migemo-init
	:hook
	;; if :commands is used, :config is not evaluated. (implicit :defer).
	;; so to call migemo-init, use :init or call it from hook.
	(after-init . migemo-init)
	:init
	(setq migemo-command
				(let* ((exec-path (cons (expand-file-name "cmigemo-default-win64" user-emacs-directory) exec-path)))
					(executable-find "cmigemo")))
	(setq migemo-options '("-q" "--emacs"))
	(setq migemo-dictionary
				(if (eq system-type 'windows-nt)
						(expand-file-name "cmigemo-default-win64/dict/utf-8/migemo-dict" user-emacs-directory)
					"/usr/share/migemo/utf-8/migemo-dict"))
	(setq migemo-user-dictionary nil)
	(setq migemo-regex-dictionary nil)
	(setq migemo-coding-system 'utf-8-unix))


;;
;; enhancement packages
;;

;; vertico. minibuffer completion UI
(use-package vertico
	:ensure t
	:init
	(setq vertico-count 15)
	(setq vertico-preselect 'first)
	(setq vertico-cycle t)

	;; case insensitive on minibuffer completion
	(setq read-file-name-completion-ignore-case t)
	(setq read-buffer-completion-ignore-case t)
	(setq completion-ignore-case t)
	:config
	(vertico-mode)

	(use-package vertico-directory
		:requires vertico
		:bind
		(:map vertico-map
					;; disable invoking dird by enter. alt, M-RET invokes dird.
					("RET" . vertico-directory-enter)
					("C-j" . vertico-directory-enter)
					("DEL" . vertico-directory-delete-char)
					("M-DEL" . vertico-directory-delete-word))
		:hook
		(rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; consult. minibuffer commands
(use-package consult
	:ensure t
	:bind (;; C-c bindings in `mode-specific-map'
				 ("C-c M-x" . consult-mode-command)
				 ;; ("C-c k" . consult-kmacro)
				 ("C-c m" . consult-man)
				 ;; org-mode
				 ;; shortcut for opening org. experiment biding
				 ("C-c A" . consult-org-agenda)
				 ("M-g o" . consult-org-heading)
				 ;; C-x bindings in `ctl-x-map'
				 ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
				 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
				 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
				 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
				 ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
				 ;;("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

				 ;; bindings for fast register access
				 ("C-q r l" . consult-register-load)
				 ("C-q r s" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
				 ("C-q r r" . consult-register)
				 
				 ;; yank
				 ("M-y" . consult-yank-pop) ;; orig. yank-pop
				 
				 ;; M-g bindings in `goto-map'
				 ;;("M-g e" . consult-compile-error)
				 ;;("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
				 ("M-g g" . consult-goto-line) ;; orig. goto-line
				 ("M-g M-g" . consult-goto-line) ;; orig. goto-line
				 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
				 ("M-g m" . consult-mark)
				 ("M-g M" . consult-global-mark)
				 ("M-g i" . consult-imenu)
				 ("M-g I" . consult-imenu-multi)
				 
				 ;; M-s bindings in `search-map'
				 ("M-s f" . consult-find)
				 ("M-s F" . consult-locate)
				 ("M-s g" . consult-grep)
				 ("M-s G" . consult-git-grep)
				 ("M-s r" . consult-ripgrep)
				 ("M-s l" . consult-line) ;; for current buffer
				 ("C-;"	 . consult-line) ;; experiment binding
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
				 ("M-s" . consult-history) ;; orig. next-matching-history-element
				 ("M-r" . consult-history) ;; orig. previous-matching-history-element
				 )

	:hook
	;; enable preview at poin in the *Completion* buffer
	(completion-list-mode . consult-preview-at-point-mode)

	:init
	(setq register-preview-delay 0.0)
	(setq register-preview-function #'consult-register-format)
	(advice-add #'register-preview :override #'consult-register-window)

	;; Use Consult to select xref locations with preview
	(setq xref-show-xrefs-function #'consult-xref)
	(setq xref-show-definitions-function #'consult-xref)

	;; Configure other variables and modes in the :config section,
	;; after lazily loading the package.
	:config
	(consult-customize
	 ;; applying emacs theme
	 consult-theme
	 :preview-key '(:debounce 0.2 any)

	 ;; preview by C-.
	 consult-ripgrep consult-git-grep consult-grep
	 consult-bookmark consult-recent-file consult-xref
	 consult--source-bookmark consult--source-file-register
	 consult--source-recent-file consult--source-project-recent-file
	 :preview-key "M-.")

	;; Optionally configure the narrowing key.
	;; Both < and C-+ work reasonably well.
	(setq consult-narrow-key "<") ;; "C-+"

	;; use projectile to grep files in a project
	(setq consult-project-function nil))


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
		:requires ido
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
		:requires ido
		:config
		(ido-ubiquitous-mode t))

	;; amx
	(use-package amx
		:ensure t
		:requires ido
		:bind ("M-x" . amx)
		:config	(amx-mode)
		;; unworked :init (amx-backend 'ido) and :config (amx-backend 'ido)
		:custom	(amx-backend 'ido)))

;; corfu. inbuffer completion UI
(use-package corfu
	:ensure t
	:bind
	(:map corfu-map
				;;("TAB" . corfu-next)
				;;("<tab>" . corfu-next)
				;; C-a is bound to corfu-prompt-begging. so replace it to move-beginning-of-line.
				([remap corfu-prompt-beginning] . move-beginning-of-line)
				("C-j" . corfu-complete)
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
	
	;; must be in :init
	(global-corfu-mode)
	:config
	(use-package kind-icon
		:ensure t
		:requires corfu
		:init
		;; disable icon
		(setq kind-icon-use-icons nil)))

;; company. traditional inbuffer completion UI
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
	(setq lsp-completion-provider :capf)

;;	;; company-fuzzy
;;	(use-package company-fuzzy
;;		:disabled
;;		:ensure t
;;		:after company
;;		:hook (company-mode . company-fuzzy-mode)
;;		;; company-fuzzy recommended using :init
;;		:init
;;		;; NOTE: install flx
;;		;;(setq company-fuzzy-sorting-backend 'flx)
;;		(setq company-fuzzy-show-annotation nil))
;;
;;
;;	;; company-fussy
;;	;; faster than company-fuzzy
;;	(use-package fussy
;;		:disabled
;;		:ensure t
;;		:config
;;		(push 'fussy completion-styles)
;;		(setq company-category-defaults nil)
;;		(setq company-category-overrides nil))
	)

;; cape. completions sources
(use-package cape
	:ensure t
	:config
	;; XXX: cape-capf-buster changes corfu previewing
	;;      (cape-capf-buster (cape-capf-super #'cape-dabbrev #'cape-file))))
	(defun my/cape-capfs ()
		(cape-wrap-super #'cape-dabbrev #'cape-file))
	(add-to-list 'completion-at-point-functions #'my/cape-capfs))

;; NOTE: currently disabled. trying prescient
;; orderless. matching for completion candidates
(use-package orderless
	:disabled
	:ensure t
	:init
	;;(setq orderless-component-separator ",")
	(setq orderless-matching-styles '(orderless-literal
																		orderless-regexp
																		orderless-prefixes))
	(setq completion-styles '(orderless basic))
	(setq completion-category-defaults nil)
	;; TODO: consider fine-grained tuning per categories
	(setq completion-category-overrides '((file (styles basic orderless)))))

;; experiment
;; prescient. matching for completion candidates
(use-package prescient
	:ensure t
	:custom-face
	(prescient-primary-highlight
	 ((t :foreground "#b3a3e9" :background "#2a273a" :weight ultra-bold)))
	:init
	;;(setq completion-styles '(basic))
	(setq completion-styles '(prescient basic))
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
		(corfu-prescient-mode))

	(use-package vertico-prescient
		:ensure t
		:after vertico
		:init
		(setq vertico-prescient-enable-filtering t)
		(setq vertico-prescient-enable-sorting t)
		:config
		(vertico-prescient-mode)))


;;
;; development packages
;;

;; ggtags. gnu global
(use-package ggtags
	:ensure t
	:bind
	("C-q g" . 'ggtags-mode))

;; dumb-jump. ensuring navigating codes work
;; NOTE: dumb-jump is registered as a xref implementation.
;;			 when lsp is not activated, M-. will use dumb-jump via xref interface.
(use-package dumb-jump
	:ensure t
	:config
	;; replace xref I/F. e.g.: M-. , C-M-. , M-?
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
	(setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; projectile. project management
(use-package projectile
	:ensure t
	;; unwork :config (projectile-mode +1)
	:init (projectile-mode +1)
	:bind-keymap
	("C-c p" . projectile-command-map))

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
		:hook (flymake-mode . flymake-diagnostic-at-point-mode)))

;; flycheck
(use-package flycheck
	:disabled
	:ensure t
	:init
	(setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
	:config
	(global-flycheck-mode))

;; yasnippet. snippet provider
(use-package yasnippet
	:disabled
	:ensure t
	:config
	;; actual snippets
	(use-package yasnippet-snippets
		:ensure t
		:requires yasnippet
		:init
		(setq yas-prompt-functions '(yas-ido-prompt))
		:config
		(yas-global-mode t)
		(yas-reload-all)))

;; experiment
;; eglot(builtin). 
(use-package eglot
	:ensure t
	:after cape
	:hook
	;; XXX: ensure use cape-file/cape-dabbrev in c/c++-mode.
	;;      not sure, but falling back to global completion-at-point-functions
	;;       won't work if it's in c/c++-mode?
	(eglot-managed-mode . (lambda ()
													(when (or (eq major-mode 'c-mode)
																		(eq major-mode 'c++-mode))
														(message "c/c++-mode fallback: modifying completion-at-point-functions")
														(setq-local completion-at-point-functions
																				(list (cape-capf-super #'eglot-completion-at-point
																															 #'cape-dabbrev
																															 #'cape-file)
																							t)))))
	:config
	(setq eglot-ignored-server-capabilities '(:hoverProvider
																						:inlayHintProvider)))

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
	(setq lsp-keymap-prefix "C-q l")
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
		:hook (lsp-mode . lsp-ui-mode)
		:bind
		(:map lsp-ui-mode-map
					;; remap xref-find-defenitions function to lsp-ui-peek-find-definitions
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

;; treesit-auto. tree-sitter lang bundles manager
(use-package treesit-auto
	:ensure t
	:config
	(setq treesit-auto-install 'prompt)
	;;(treesit-auto-add-to-auto-mode-alist 'all)
	(global-treesit-auto-mode))


;;
;; major-modes
;;

;; cc-mode(builtin)
(use-package cc-mode
	:ensure t
	;;:after (:and (:any lsp-mode eglot) cape)
	:after cape
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
		:requires cc-mode
		:bind
		(("C-q f b" . clang-format-buffer)
		 ("C-q f r" . clang-format-region))
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
	(python-mode . eglot-ensure))

;; elisp-mode
;;(use-package elisp-mode
;;	:after cape
;;	:hook
;;	(emacs-lisp-mode . (lambda ()
;;											 (setq-local completion-at-point-functions
;;																	 (list cape-dabbrev)))))
;;																		cape-elisp-symbol
;;																		cape-dabbrev
;;																		elisp-completion-at-point
;;																		t)))))
;;)

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


;;
;; theme config
;;

(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
					 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))

;; theme for linux
(use-package doom-themes
	:if (eq system-type 'gnu/linux)
	:ensure t
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
	:config
	(load-theme 'ef-tritanopia-dark t))


;;
;; customizations el files
;;

;; load autosaved customizations from ~/.emacs.d/lisp/autosaved-custom.el
(load "autosaved-custom" t)

;; load customizations for work
(load "work" t)


;;(profiler-report)
;;(profiler-stop)

;;;; EOF

