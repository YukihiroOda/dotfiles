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
(add-to-load-path "elisp")

;; To use Powershell.
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
 (autoload 'powershell "powershell.el" "Run powershell as a shell within emacs." t))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://www.google.co.jp/search?q=")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("elpa" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages (quote (proof-general haskell-mode tuareg yatex)))
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
