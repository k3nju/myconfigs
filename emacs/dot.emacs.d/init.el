;; コメント入力マクロ
(defun my-macro-insline ()
  (interactive)
  (insert-string
   (if my-comment-prefix
	   (concat my-comment-prefix
			   (make-string (- 80 (length my-comment-prefix))
							?-))))
  ) ;; end of my-macro-insline

;; 各モードで共通実行するファイル
(defun my-common-configure ()
  ;;
  ;; 非標準パッケージの読み込み
  ;;
  
  ;; load-path設定
  (add-to-list 'load-path "~/.emacs.d/lisp")
  
  ;; package (2.4から標準で入ったようだが古い環境でも使うので)
  (when (require 'package nil t)
	(add-to-list 'package-archives '("melpa"		.	"http://melpa.org/packages/"))
	(add-to-list 'package-archives '("marmalade"	.	"http://marmalade-repo.org/packages/"))
	(package-initialize))

  ;; auto-complete
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
	(global-auto-complete-mode t)
	(define-key ac-completing-map (kbd "C-n") 'ac-next)
	(define-key ac-completing-map (kbd "C-p") 'ac-previous)
	(define-key ac-completing-map (kbd "C-m") 'ac-complete))

  ;; redo+	 
  (when (require 'redo+ nil t)
	(global-set-key (kbd "C-_") 'redo))
  
  ;;
  ;; フレーム、ウィンドウ、表示系の設定
  ;;

  ;; tool-var, tooltop, tooltipを表示させない
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (menu-bar-mode nil)
  
  ;; スタートアップメッセージを表示しない
  (setq inhibit-startup-message)

  ;; 行番号表示
  (global-linum-mode  t)
  (setq linum-format "%4d ")
  
  ;; モードラインに行/列番号表示
  (line-number-mode t)
  (column-number-mode t)
  
  ;; 折り返し表示しない
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t) ;; window分割時
  
  ;; カーソルを点滅表示させない
  (blink-cursor-mode 0)
  
  ;; 対応するかっこを強調表示
  (show-paren-mode 1)

  ;; 検索時に大文字小文字を無視
  (setq case-fold-search t)
  
  ;; タブ設定
  (setq default-tab-width 4)
  (setq tab-width 4) ;; local mode
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
  
  ;;
  ;; キーバインド
  ;;

  ;; delキーで先方向の削除
  (global-set-key "\177" 'delete-backward-char)
  ;; C-hで先方向の削除
  (global-set-key "\C-h" 'delete-backward-char)
  ;; Meta + 1でmy-macro-inslineを実行
  (global-set-key "\M-1" 'my-macro-insline)
  
  ;; emacsがver24以上だったら、electric-indent-modeを無効化する
  ;; (23まではC-j => new-line-and-indent、enter => new-lineだったが、
  ;;  emacs24.4からC-jとenterの挙動が入れ替わった)
  (if (>= emacs-major-version 24)
	  (setq electric-indent-mode  nil))

  ;;  
  ;; 文字コード、言語設定
  ;;

  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)

  ;;
  ;; フォント、色設定
  ;; 

  ;; フォントロックモードを自動的に有効にする
  (global-font-lock-mode t)

  ;; カスタムテーマのロード
  (when (file-exists-p "~/.emacs.d/themes/")
	(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

	;; solarized-dark
	(when (load-theme 'solarized-dark t)
	  '(font-lock-comment-delimiter-face ((t (:foreground "green4"))))
	  '(font-lock-comment-face ((t (:foreground "green4"))))
	  '(font-lock-string-face ((t (:foreground "OrangeRed3"))))))
  );; end of my-common-configure
(my-common-configure)
 
;;
;; C/C++の設定
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
			 ;; whitesmithsを継承しない
			 ;; whitesmithsは、"if("を入力した時点でインデントし、
			 ;; "...)"まで入力した時点でインデントをundoする動作を行う
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

;; C/C++モードのフック関数
(defun my-c-mode-hook ()
  (setq my-comment-prefix "//")
  (c-set-style "whitesmiths-kai"))

;; C/C++モードフック登録
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; .h .rlをC++モードで開く
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rl\\'" . c++-mode))

;;
;; Pythonの設定
;;

;; Pythonモードのフック関数
(defun my-python-mode-hook ()
  (setq my-comment-prefix "#")
  (setq python-indent 4))
  


;; Pythonモードフックの登録
(add-hook 'python-mode-hook 'my-python-mode-hook)

							
;;
;; Goの設定
;;

;; Goモードのフック関数
(defun my-go-mode-hook ()
  (setq my-comment-prefix "//")
  (require 'go-autocomplete nil t)
  ;; gofmtの代わりにgoimports使う
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Goモードフックの登録
(add-hook 'go-mode-hook 'my-go-mode-hook)
