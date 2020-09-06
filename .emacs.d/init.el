;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;;Define the function to add load-path.

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; Add ~\.emacs.d\elisp to load-path.
(add-to-load-path "elisp" "elpa")

(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Settings to use only in Windows system.
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  ;; To use Powershell.
  (autoload 'powershell "powershell.el" "Run powershell as a shell within emacs." t)
  ;; To use coq
  (setq coq-prog-name "C:/Program Files (x86)/Coq/bin/coqtop.exe")
  ;; To use utf-8 in Windows
  (unless (cl-member 'cp65001 coding-system-list)
    (define-coding-system-alias 'cp65001 'utf-8))
  ;; To use wsl
  (setq shell-file-name (executable-find "bash"))
  (setq grep-use-null-device nil)
  ;; To use aspell
  (setq-default ispell-program-name "~/.emacs.d/aspell.cmd")
  (with-eval-after-load "ispell"
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  ;; 半角英字設定
  (set-face-attribute 'default nil :family "Roboto Mono" :height 125)
  ;; 全角かな設定
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "游ゴシック" :size 14))
  )

;; Settings to use only in Linux system.
(when (eq system-type 'gnu/linux)
  ;; 半角英字設定
  (set-face-attribute 'default nil :family "Roboto Mono" :height 150)
  ;; 全角かな設定
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "IPAゴシック" :size 20))
  ;; To use aspell
  (setq-default ispell-program-name "aspell")
  (with-eval-after-load "ispell"
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  ;; mozc
  (require 'mozc)
  (setq default-input-method "japanese-mozc")     ; IMEをjapanes-mozcに
  (add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "red")))
  (add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "black")))
  )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://www.google.com/search?q=-sejuku+-wa3.i-3-i.info+")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (mozc which-key free-keys elscreen csv-mode volatile-highlights evil async caml dash git-commit popup transient with-editor magit yatex markdown-mode git-gutter auctex auto-complete proof-general haskell-mode tuareg)))
 '(tool-bar-mode nil))

;; Screen display control
(set-face-background 'mode-line-inactive "gray60")  
(global-hl-line-mode t)
(set-face-background 'hl-line "lavender")
(show-paren-mode t)
(setq show-paren-style 'mixed)
(set-face-attribute 'show-paren-match nil
                    :background "medium spring green")

(transient-mark-mode t)
(set-face-background 'region "Orange")

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;elscreen
(setq elscreen-prefix-key (kbd "C-!"))
(elscreen-start)
(global-set-key (kbd "<f9>") 'elscreen-toggle)

;; magit-status key bind
(global-set-key (kbd "C-x g") 'magit-status)
;; compile key bind
(global-set-key (kbd "C-x c") 'compile)
;; Change window previous
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))


;; To use git-gutter
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  )

;;auto-complete
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-fuzzy t)          ;; 曖昧マッチ  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))


;;; YaTeX-mode ------------------------------
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq makeindex-command "mendex")
(setq auto-mode-alist (cons (cons ".tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons ".ltx$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons ".sty$" 'yatex-mode) auto-mode-alist))
(setq YaTeX-kanji-code 4) ;utf-8
(setq YaTeX-need-nonstop t)
;; yatex-mode-hook の設定
(add-hook 'yatex-mode-hook
	  (lambda () (outline-minor-mode t)
	    (setq LaTeX-outline-regexp
		  (concat "[ t]*" (regexp-quote "")
			  "(appendix|documentstyle|part|chapter|" "section|subsection|subsubsection|" "paragraph|subparagraph)" "*?[ t]*[[{]"))
	    (make-local-variable 'outline-regexp) ))
(setq dvi2-command "xdvi -geo +0+0 -s 6")

;;; YaHTML-mode -----------------------------------------
(setq auto-mode-alist
      (cons (cons ".html$" 'yahtml-mode) auto-mode-alist))
(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)

;;; AUCTeX-mode ------------------------------
(require 'tex-site)
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jsarticle")
(setq-default TeX-master nil)
(setq TeX-parse-self t)

;;; BibTeX-mode -------------------------------------------------------
(setq bibtex-user-optional-fields
      '(("annote" "Personal annotation (ignored)")
	("yomi" "Yomigana")
	("location" "where it is (ignored)")
	("memo" "Memorundum (ignored)")
	))

;; To use the Coq Emacs mode, you need to put the following lines in
;; your .emacs file:
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; flyspell-mode -------------
(setq ispell-list-command "--list")
(mapc
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(;; ここに書いたモードではコメント領域のところだけflyspell-mode が有効になる
   c-mode-common-hook
   emacs-lisp-mode-hook
   sh-mode-hook
   ))
(mapc
 (lambda (hook)
   (add-hook hook (lambda () (flyspell-mode 1))))
 '( ;; ここに書いたモードではflyspell-mode が有効になる
   yatex-mode-hook
   bibtex-mode-hook
   git-commit-setup-hook
   git-commit-turn-on-flyspell
   org-mode-hook
   ))

;; evil-mode
(global-set-key (kbd "C-M-?") 'evil-mode)

;;C-x C-e eval

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
