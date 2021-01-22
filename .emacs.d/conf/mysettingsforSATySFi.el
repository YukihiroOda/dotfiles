;; Settings for SATySFi
(require 'satysfi)
(add-to-list 'auto-mode-alist '("\\.saty$" . satysfi-mode))
(add-to-list 'auto-mode-alist '("\\.satyh$" . satysfi-mode))

;; set the command for typesetting (default: "satysfi -b")
(setq satysfi-command "satysfi")

; ; set the command for opening PDF files (default: "open")
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (setq satysfi-pdf-viewer-command "sumatrapdf")
  )
(when (eq system-type 'gnu/linux)
  (setq satysfi-pdf-viewer-command "evince")
  )
