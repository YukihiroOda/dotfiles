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

