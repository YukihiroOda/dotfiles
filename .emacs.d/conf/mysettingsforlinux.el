;; Settings to use only in Linux system.
(when (eq system-type 'gnu/linux)
  ;; 半角英字設定
  (set-face-attribute 'default nil :family "SourceHanSansHW" :height 120)
  ;; 全角日本語設定
  (set-fontset-font (frame-parameter nil 'font)
                    'unicode
                    (font-spec :family "SourceHanSansHW" :size 17))
  ;; To use aspell
  (setq-default ispell-program-name "aspell")
  (with-eval-after-load "ispell"
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  ;; mozc
  (require 'mozc)
  ;; IMEをjapanes-mozcに
  (setq default-input-method "japanese-mozc")
  ;; mozc を使っている間のカーソルの色を変更
  (add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "red")))
  (add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "black")))
  )
