;; lang and coding system
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;
;; Comment macro
;;
(defun my-macro-insline ()
  (interactive)
  (if (boundp 'my-comment-prefix)
	  (insert-string
	   (concat my-comment-prefix
			   (make-string (- 80 (length my-comment-prefix))
							?-))))
  ) ;; end of my-macro-insline

;;
;; Load packages
;;
(defun my-load-packages ()
  (add-to-list 'load-path "~/.emacs.d/lisp")
  
  ;; package
  (when (require 'package nil t)
	(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
	(package-initialize))

  ;; auto-complete
  ;; NOTE: Requires auto-complete package
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
	(global-auto-complete-mode t)
	(ac-config-default)
	(define-key ac-completing-map (kbd "C-n") 'ac-next)
	(define-key ac-completing-map (kbd "C-p") 'ac-previous)
	(define-key ac-completing-map (kbd "C-m") 'ac-complete))

  ;; helm
  ;; NOTE: Requires helm package
  ;;(when (require 'helm-config nil t)
	;;(helm-mode t)
	;;(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
	;;(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;;)
  
  ) ;; end of my-load-packages
(my-load-packages)

;;
;; Key binding
;;
(defun my-set-key-bindings ()
  ;; [del] key to delete backword char
  (global-set-key "\177" 'delete-backward-char)
  ;; C-h to delete backword char
  (global-set-key "\C-h" 'delete-backward-char)
  ;; Execute my-macro-insline
  (global-set-key "\M-1" 'my-macro-insline)
  
  ;; emacsがver24以上だったら、electric-indent-modeを無効化する
  ;; Disable electric-indent-mode if emacs version is greater than or equal to 24
  ;; NOTE: until emacs version 23, C-j is assigned to new-line-and-indent,
  ;;        and [enter] is assigned to enter.
  (if (>= emacs-major-version 24)
	  (setq electric-indent-mode  nil))
);; end of my-set-key-bindings
(my-set-key-bindings)

;;
;; Load theme
;;
(defun my-load-theme ()
  (when (and (file-exists-p "~/.emacs.d/themes/")
			 (boundp 'custom-theme-load-path))
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

	;; load theme
	(load-theme 'solarized-dark t))
  
  );; end of my-load-theme
(my-load-theme)

;;
;; C/C++ configs
;;

;; Whitesmiths-kai style
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
			 '(;; Indentation configuration
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


;; C/C++ mode hook function
(defun my-c-mode-hook ()
  (setq my-comment-prefix "//")
  ;;(c-set-style "whitesmiths-kai")
  )

;; Register C/C++ mode hook
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
;; Use google C++ coding style.
;; NOTE: Requires google-c-style package
(add-hook 'c-mode-common-hook 'google-set-c-style)
;; global ggtag
;; NOTE: Requires ggtags package
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++mode 'java-mode)
			  (ggtags-mode 1))))

;; Open .h .rl files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rl\\'" . c++-mode))

;;
;; Python configs
;;

;; Python mode hook function
(defun my-python-mode-hook ()
  (setq my-comment-prefix "#")
  (setq python-indent 4))

;; Register python mode hook
(add-hook 'python-mode-hook 'my-python-mode-hook)

;;
;; Go configs
;;
;; NOTE: Requires gocode, goimports and add them to PATH(environment variables)
;;       Requires go-autocomplete.el
(when (require 'go-mode-autoloads nil t)
  (require 'go-autocomplete nil t)

  ;; Go mode hook function
  (defun my-go-mode-hook ()
	(setq my-comment-prefix "//")
	;; Use goimports, instead of gofmt
	(setq gofmt-command "goimports")
	;; Run goimports when saving files
	(add-hook 'before-save-hook 'gofmt-before-save))
  
  ;; Register Go mode  hook
  (add-hook 'go-mode-hook 'my-go-mode-hook))

;;
;; perl
;;
(defun my-perl-mode-hook ()
  (custom-set-faces
   '(font-lock-variable-name-face ((t (:foreground "#0404B4"))))))
(add-hook 'perl-mode-hook 'my-perl-mode-hook)


;;
;; Define customized variables
;;
(custom-set-variables
 ;; Don't display startup-message, tool-bar, menu-bar, tooltip
 '(inhibit-startup-message 0)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(tooltip-mode nil)

 ;; Display line number
 '(global-linum-mode  t)
 '(linum-format "%4d ")

 ;; Display line number, column number in the mode line
 '(line-number-mode t)
 '(column-number-mode t)

 ;; Don't truncate lines
 '(truncate-lines t)
 '(truncate-partial-width-windows nil) ;; for split window

 ;; Don't blink cursor
 '(blink-cursor-mode nil)

 ;; Highlight pair parentheses
 '(show-paren-mode t)

 ;; search in case-insensitive
 '(case-fold-search t)

 ;; tab
 '(tab-width 4)
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 )

;;
;; Define faces
;;
(custom-set-faces
 '(font-lock-comment-delimiter-face ((t (:foreground "#008b00"))))
 '(font-lock-comment-face ((t (:foreground "#008b00"))))
 '(font-lock-string-face ((t (:foreground "#a42c00")))))

