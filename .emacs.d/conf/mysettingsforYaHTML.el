;;; YaHTML-mode -----------------------------------------
(setq auto-mode-alist
      (cons (cons ".html$" 'yahtml-mode) auto-mode-alist))
(autoload 'yahtml-mode "yahtml" "Yet Another HTML mode" t)
