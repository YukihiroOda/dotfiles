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

(load-file "~/.emacs.d/elpa/proof-general-20190212.1433/generic/proof-site.el")
;; Settings to use only in Windows system.
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  ;; To use Powershell.
  (autoload 'powershell "powershell.el" "Run powershell as a shell within emacs." t)
  ;; To use coq
  (setq coq-prog-name "C:/Program Files (x86)/Coq/bin/coqtop.exe")
  )

(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://www.google.co.jp/search?q=")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (yatex auctex auto-complete git-gutter magit proof-general haskell-mode tuareg)))
 '(tool-bar-mode nil))

;; magit-status key bind
(global-set-key (kbd "C-x g") 'magit-status)

;; compile key bind
(global-set-key (kbd "C-x c") 'compile)


;; To use git-gutter
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  )

;; show-paren-mode
 (show-paren-mode t)

;;auto-complete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))


;;; YaTeX-mode ------------------------------
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq makeindex-command "mendex")
(setq auto-mode-alist (cons (cons ".tex$" 'yatex-mode) auto-mode-alist))
(setq YaTeX-kanji-code 3) ;EUC-code
(setq YaTeX-need-nonstop t)
;; yatex-mode-hook の設定
(add-hook 'yatex-mode-hook
	  (lambda () (outline-minor-mode t)
	    (setq LaTeX-outline-regexp
		  (concat "[ t]*" (regexp-quote "")
			  "(appendix|documentstyle|part|chapter|" "section|subsection|subsubsection|" "paragraph|subparagraph)" "*?[ t]*[[{]"))
	    (make-local-variable 'outline-regexp) ))
(setq dvi2-command "xdvi -geo +0+0 -s 6")

;;; AUCTeX-mode ------------------------------
(require 'tex-site)
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jsarticle")
(setq-default TeX-master nil)
(setq TeX-parse-self t)

;; To use the Coq Emacs mode, you need to put the following lines in
;; your .emacs file:
 (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
 (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; 半角英字設定
(set-face-attribute 'default nil :family "Consolas" :height 125)
;; 全角かな設定
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   (font-spec :family "IPAゴシック" :size 14))

;;C-x C-e eval
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
