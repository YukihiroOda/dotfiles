;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

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
(add-to-load-path "elisp" "elpa" "conf" "secret")

;;言語設定
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; custom-set-variables
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

;; Settings to use only in Windows system.
(load "mysettingsforwindows")

;; Settings to use only in Linux system.
(load "mysettingsforlinux")

;; Settings for screen
(load "mysettingsforscreen")

;; Settings for git
(load "mysettingsforgit")

;; Settings for auto-complete
(load "mysettingsforauto-complete")

;; Settings for TeX
(load "mysettingsforTeX")

;; Settings for SATySFi
(load "mysettingsforSATySFi")

;; Settings for YaHTML
(load "mysettingsforYaHTML")

;; Settings for Coq
(load "mysettingsforcoq")

;; Settings for spelling
(load "mysettingsforspelling")

;; Settings for evil
(load "mysettingsforevil")

;; Settings for Media Wiki
(load "mysettingsforMediaWiki")

;;C-x C-e eval

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
