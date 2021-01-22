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
