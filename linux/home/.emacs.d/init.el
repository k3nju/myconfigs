;; coding system

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)


;; path config 
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; theme config
(when (and (file-exists-p (expand-file-name "themes" user-emacs-directory))
           (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)))
(when (getenv "DISPLAY")
  (load-theme 'solarized-dark t))


;; key bindings
;; set "C-h" as delete-backward-char
(global-set-key (kbd "C-h") 'delete-backward-char)
;; enable cursor to move with M-p and M-p among windows
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-p") '(lambda ()
			       (interactive)
			       (other-window -1)))


;; mode config
;; disable electric-indent-mode always
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
;; winner-mode
(winner-mode t)


;; package config
(when (require 'package nil t)
  (setq package-archives
		    '(("gnu" . "http://elpa.gnu.org/packages/")
		      ("melpa" . "http://melpa.org/packages/")
		      ("org" . "http://orgmode.org/elpa/")))
  (package-initialize))


;; use-package
(unless (package-installed-p 'use-package)
  (message "use-package is not installed and installing it") 
  (package-refresh-contents)
  (package-install 'use-package))


;; window-number
(use-package window-number
  :ensure t
  :config (window-number-meta-mode))


;; wgrep
(use-package wgrep
  :ensure t)


;; company
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
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
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))


;; google-c-style
(use-package google-c-style
  :ensure t
  :defer 1
  :init (add-hook 'c-mode-common-hook 'google-set-c-style))


;; lsp-mode
;(use-package lsp-mode
;  :ensure t
;  :commands lsp)


;; lsp-ui
;(use-package lsp-ui
;  :commands lsp-ui-mode)


;; company-lsp
;(use-package company-lsp
;  :commands company-lsp)


;; customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen 0)
 '(line-number-mode t)
 '(linum-format "%4d ")
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (google-c-style company window-number wgrep use-package ggtags)))
 '(show-paren-mode t)
 '(tab-stop-list (number-sequence 2 120 2))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:foreground "#008b00"))))
 '(font-lock-comment-face ((t (:foreground "#008b00"))))
 '(font-lock-string-face ((t (:foreground "#a42c00")))))



