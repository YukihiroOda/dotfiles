;;auto-complete
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
  (setq ac-use-fuzzy t)          ;; 曖昧マッチ  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))
