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
    (auto-complete git-gutter magit proof-general haskell-mode tuareg yatex)))
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



;; To use the Coq Emacs mode, you need to put the following lines in
;; your .emacs file:
 (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
 (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;C-x C-e eval
(put 'downcase-region 'disabled nil)
