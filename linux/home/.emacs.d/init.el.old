;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lang and coding system
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Load packages
(add-to-list 'load-path "~/.emacs.d/lisp")
  
;; package
(when (require 'package nil t)
  (package-initialize)
  (setq package-archives
		'(("gnu" . "http://elpa.gnu.org/packages/")
		  ("melpa" . "http://melpa.org/packages/")
		  ("org" . "http://orgmode.org/elpa/"))))

;; company-mode
;; NOTE: Requires company package
(when (require 'company nil t)
  (add-hook 'after-init-hook 'global-company-mode)
  
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-m") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)

  (setq company-idle-delay 0))

;; helm
;; NOTE: Requires helm package
;;(when (require 'helm-config nil t)
;;(helm-mode t)
;;(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
;;(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;;)


;; ido-mode
;(ido-mode t)
;(ido-everywhere t)
;(setq ido-enable-flex-matching t)

;; anything.el
;; NOTE: require anything.el package
(when (require 'anything-config nil t)
  (setq anything-enable-shortcuts 'prefix)
  (define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
  (global-set-key (kbd "C-x b") 'anything-mini)
  
  ;; anything-git-files.el
  ;; NOTE: require anything-git-files.el
  (when (require 'anything-git-files)
    (global-set-key (kbd "C-x g")
                    (lambda ()
                      (interactive)
                      (and (anything-git-files:git-p)
                           (anything-git-files))))))


;; point-undo.el
;; NOTE: require point-undo.el package
(when (require 'point-undo nil t)
  (global-set-key (kbd "C-\\") 'point-undo) ;; experimental binding
  (global-set-key [f7] 'point-undo)
  (global-set-key (kbd "M-\\") 'point-redo) ;; experimental binding
  (global-set-key [S-f7] 'point-redo))


;; window-number
;; NOTE: require window-number package
;; move window M-1 .. M-9
(when (require 'window-number nil t)
  (window-number-meta-mode))
  
;;
;; default key bindings
;;

;; [del] key to delete backword char
(global-set-key "\177" 'delete-backward-char)

;; C-h to delete backword char
(global-set-key "\C-h" 'delete-backward-char)

;; emacsがver24以上だったら、electric-indent-modeを無効化する
;; Disable electric-indent-mode if emacs version is greater than or equal to 24
;; NOTE: until emacs version 23, C-j is assigned to new-line-and-indent,
;;        and [enter] is assigned to enter.
(if (>= emacs-major-version 24)
	(setq electric-indent-mode  nil))

;; wrap around cursor window moving
(setq windmove-wrap-around t)

;; window moving
;; M-n = next(other) window. same to C-x o
;; M-p = previous window
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") '(lambda ()
							   (interactive)
							   (other-window -1)))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; wgrep
(require 'wgrep nil t)

;; themes config
(when (and (file-exists-p "~/.emacs.d/themes/")
		   (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/"))
;; use solarized-dark
(when (getenv "DISPLAY")
  (load-theme 'solarized-dark t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(when (require 'google-c-style nil t)
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; ggtags for gnu global
;; NOTE: Requires ggtags package
(when (require 'ggtags nil t)
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
				(ggtags-mode 1)))))

;; Open .h .rl files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rl\\'" . c++-mode))

;;
;; Python configs
;;
;; NOTE: Requires company-jedi
;; NOTE: Requires jedi, epc python package(pip install jedi epc)
;; Installation:
;;   mkdir ~/.emacs.d/.python-environments/
;;   python -m venv ~/.emacs.d/.python-environments/default
;;   ~/.emacs.d/.python-environments/default/bin/activate
;;   pip install jedi epc
;;   run emacs and "M-x jedi:install-server"

;; Python mode hook function
(defun my-python-mode-hook ()
  (when (require 'jedi-core nil t)
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t)
    (add-to-list 'company-backends 'company-jedi))
  
  (setq python-indent 4))
;; Register python mode hook
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)


;;
;; Go configs
;;
;; NOTE: Requires gocode, goimports and add them to PATH(environment variables)
;; NOTE: Requires company-go package

;; Go mode hook function
(defun my-go-mode-hook ()
  (when (require 'company-go nil t)
    ;; tell company-mode to use comany-go as backends
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))

  ;; use godef command instead of tags
  (local-set-key (kbd "M-.") 'godef-jump)
  
  ;; golang uses tab and witdh is 4
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  
  ;; Use goimports, instead of gofmt
  (setq gofmt-command "goimports")
  ;; Run goimports when saving files
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Register Go mode hook
(add-hook 'go-mode-hook 'my-go-mode-hook)


;;
;; perl
;;
(defun my-perl-mode-hook ()
  (custom-set-faces
   '(font-lock-variable-name-face ((t (:foreground "#0404B4"))))))
(add-hook 'perl-mode-hook 'my-perl-mode-hook)


;;
;; javascript
;;
(defun my-js-mode-hook ()
	(setq indent-tabs-mode nil)
	(setq js-indent-level 2))
(add-hook 'js-mode-hook 'my-js-mode-hook)
;; elm-lang
(defun my-elm-mode-hook ()
  ;; currently not working.
  ;; ???: need to npm -g elm-oracle?
  (add-to-list 'company-backends 'company-elm))
(add-hook 'elm-mode-hook 'my-elm-mode-hook)

;;
;; my customized variables
;;


;;
;; Define customized variables
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen 0)
 '(line-number-mode t)
 '(linum-format "%4d ")
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (company-jedi company-go company elm-mode anything-git-files wgrep web-mode vue-mode window-number magit point-undo anything package-utils solarized-theme google-c-style ggtags)))
 '(show-paren-mode t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil))

;;
;; Define faces
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:foreground "#008b00"))))
 '(font-lock-comment-face ((t (:foreground "#008b00"))))
 '(font-lock-string-face ((t (:foreground "#a42c00")))))

