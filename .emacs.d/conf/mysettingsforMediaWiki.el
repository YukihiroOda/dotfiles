;; Settings for Media Wiki
(require 'mediawiki)
(setq auto-mode-alist (cons (cons ".mediawiki$" 'mediawiki-mode) auto-mode-alist))
(setq mediawiki-mode-hook (lambda ()
			    ;; ...
                            (turn-off-auto-fill)
			    ))
(define-key mediawiki-mode-map (kbd "C-x C-s") nil)
(define-key mediawiki-mode-map (kbd "C-x C-s") 'save-buffer)
(setq browse-url-generic-program (executable-find "google-chrome")
browse-url-browser-function 'browse-url-generic)
