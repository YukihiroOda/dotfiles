;; To use the Coq Emacs mode, you need to put the following lines in
;; your .emacs file:
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
