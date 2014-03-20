;;
;; common
;;
(defun personal-global-config ()
  ;; Add load-path
  (add-to-list 'load-path "/usr/share/emacs/site-lisp" t) ;; system local
  (add-to-list 'load-path "~/.emacs.d") ;; user local
  
  ;;
  ;; Misc
  ;;

  ;; Disable startup-message
  (setq inhibit-startup-message t)
  ;; Title bar format
  (setq frame-title-format
	;; %b = buffer name
	(format "%%b - emacs@ %s"
		system-name))
  ;; Print line number
  (setq line-number-mode t)
  (linum-mode)
  ;; Print column number
  (setq column-number-mode t)
  ;; Don't truncate lines
  (setq truncate-lines t)
  ;; Don't truncate lines in window mode
  (setq truncate-partial-width-windows t)
  ;; Dont' blink cursor
  (blink-cursor-mode nil)
  ;; Highlight pair strings(ex. {and})
  (show-paren-mode 1)
  ;; font-lock-mode(enables keyword coloring or highlighting) is used in minor mode,
  ;; enabling it automatically needs to call global-font-lock-mode passed with t
  ;; fboundp returns symbol is bind with function or not
  (when (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
  ;; Ignore case in searching
  (setq case-fold-search t)
  ;; Don't show tool-bar, menu-bar, tooltip
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  
  ;;
  ;; tab configuration
  ;;
  
  ;; default-tab-width configures default vale of tab-width
  (setq default-tab-width 4)
  ;; default value of tab-width(No need to be set if default-tab-width is configured?)
  (setq-default tab-width 4)
  ;; List of position where cursor stops at when tab entered
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
  ;; Replace indents to tabs when indent depth matchs to one of tab-width
  (setq-default indent-tabs-mode t)

  ;;
  ;; global key map configuration
  ;;
  
  ;; use "del" key to delete backward char.
  (global-set-key "\177" 'delete-backward-char)
  ;; use "Ctrl + h" to delete backward char
  (global-set-key "\C-h" 'delete-backward-char)
  ;; Meta + 1でmacro-f1を実行
  ;; use "Meta + 1" to execute macro-f1
  (global-set-key "\M-1" 'macro-f1)
)
(personal-global-config)


;;
;; Common coloring 
;;
(defun personal-set-color ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  ;;(load-theme 'solarized-dark t)
  (load-theme 'blue-sea t)
)
(personal-set-color)

;;
;; C/C++ mode configuration
;;

;; .h/.rl files are opened with C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; My C++ coding style.
;; Whitesmiths-kai
;;
;; namespace
;;     {           <- (namespace-open . +)
;;     class Hoge  <- (innamespace . 0)
;;         {       <- (class-open . +)
;;             int i;   <- (inclass . +)
;;         private:     <- (access-label . -)
;;             void F(int n)
;;                 {         <- (defun-open . +)
;;                 int y(0); <- (defun-block-intro . 0)
;;                 if(!n)
;;                     {     <- ??? substatement-open?
;;                     ++n;  <- (statement-block-intro . 0)
;;                     }
;;
;;                 switch(y+n)
;;                     {        <- ???
;;                     case 1:
;;                         {    <- (statement-case-open . +)
;;                         goto label;
;;                         }
;;                     }
;;
;;                 {      <- ???
;;                 label: <- (label . 0)
;;                 }
;;
;;                 int z[] =
;;                     {   <- (brace-list-open . +)
;;                     1,  <- (brace-list-intro . 0)
;;                     2,  <- ??? brace-list-entry?
;;                     };  <- (brace-list-close . 0)
;;
;;                 }      <- (defun-close . 0)
;;         }       <- (class-close . +)
;;     }           <- (namespace-close . +)
;;
;; extern "C"
;;     {         <- (extern-lang-open . +)
;;     void X(); <- (inextern-lang . 0)
;;     }         <- (extern-lang-close . 0)
;;
(c-add-style "whitesmiths-kai"
			 ;; Don't inherit whitesmith because,
			 ;; It indents when entered "if(", and undo indents when "exp..)".
			 ;; This behavior is dirty-looking.
			 '(;; Color configuration
			   (personal-set-color)
			   ;; Indentation configuration
			   (c-offsets-alist
				;; namespace
				(namespace-open . +) 
				(namespace-close . +)
				(innamespace . 0)
				;; brace
				(brace-list-open . +)
				(brace-list-close . 0)
				(brace-list-intro . 0)
				;; class
				(class-open . +)
				(class-close . +)
				(inclass . +)
				;; C++ class access label
				(access-label . -)
				;; function
				(defun-open . +)
				(defun-close . 0)
				(defun-block-intro . 0)
				;; switch-case block
				(statement-case-open . +)
				;; any other blocks. ex(if,for,do-while)
				(statement-block-intro . 0)
				;; label
				(label . 0)
				;; extern "C"
				(extern-lang-open . +)
				(extern-lang-close . +)
				(inextern-lang . 0))))

;; Hook function for C/C++ mode
(defun personal-c-mode-hook ()
  (c-set-style "whitesmiths-kai"))

;; Macro for inserting delimiter line.(dedicated to C++)
(defun macro-f1 ()
  (interactive)
  (insert-string "//-----------------------------------------------------------------------------------------//")
  )

;; Add C/C++ hook function
(add-hook 'c-mode-common-hook 'personal-c-mode-hook)


;;
;; Python mode configuration
;;

;; Hook function for python mode
(defun personal-python-mode-hook ()
  (setq python-indent 4)
)

;; Add python mode hook function
(add-hook 'python-mode-hook 'personal-mode-hook)

;;
;; Go mode configuration 
;;
(defun personal-go-mode-hook ()
  (personal-global-config)
  ;;  (local-set-key (kbd "M-.") 'godef-jump)
  ;;  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  ;;  (local-set-key (kbd "C-c i" ) 'go-goto-imports)
  ;;  (local-set-key (kbd "C-c d" ) 'godoc())
)

(when (require 'go-mode-load nil t)
  ;; Compile when saving sources
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Add Go mode hook function
  (add-hook 'go-mode-hook 'personal-go-mode-hook))
