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

;; Default screen size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;elscreen
(setq elscreen-prefix-key (kbd "C-!"))
(elscreen-start)
(global-set-key (kbd "<f9>") 'elscreen-toggle)
