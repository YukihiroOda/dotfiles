;; Settings to use only in Windows system.
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  ;; To use Powershell.
  (autoload 'powershell "powershell.el" "Run powershell as a shell within emacs." t)
  ;; To use coq
  (setq coq-prog-name "C:/Program Files (x86)/Coq/bin/coqtop.exe")
  ;; To use utf-8 in Windows
  (unless (cl-member 'cp65001 coding-system-list)
    (define-coding-system-alias 'cp65001 'utf-8))
  ;; To use wsl
  (setq shell-file-name (executable-find "bash"))
  (setq grep-use-null-device nil)
  ;; To use aspell
  (setq-default ispell-program-name "~/.emacs.d/aspell.cmd")
  (with-eval-after-load "ispell"
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  ;; 半角英字設定
  (set-face-attribute 'default nil :family "Roboto Mono" :height 125)
  ;; 全角かな設定
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "游ゴシック" :size 14))
  )
