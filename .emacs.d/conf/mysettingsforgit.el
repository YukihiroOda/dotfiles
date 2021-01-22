;; magit-status key bind
(global-set-key (kbd "C-x g") 'magit-status)
;; compile key bind
(global-set-key (kbd "C-x c") 'compile)
;; Change window previous
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))


;; To use git-gutter
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  )
