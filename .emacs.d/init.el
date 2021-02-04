;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.e
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;;leaf settings ---------------
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
		       ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Define the function to add load-path.

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; Add ~\.emacs.d\elisp to load-path
(add-to-load-path "elisp" "elpa" )

;;Language settings -------------
(leaf Language
  :ensure mozc
  :config
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  )

;; leaf-convert and leaf-tree ----------------
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))
  )

;; macrostep ----------------
(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; custom-set-variables ------------------
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  )
(leaf custom-set-variables
  :custom (
	   (eww-search-prefix . "https://www.google.com/search?q=-sejuku+-wa3.i-3-i.info+")
	   (inhibit-startup-screen . t)
	   (js-indent-level . 2)
	   (make-backup-files)
	   (tool-bar-mode)
	   )
  )

;; autorevert --------------
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;; cc-mode
(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  )

;; delsel
(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode
  )

;; kill-ring
(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil))
  )

;; ivy
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t)
  )

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

;; Which-key
(leaf which-key
  :ensure which-key
  )
;; Free-key
(leaf free-keys
  :ensure free-keys
  )

;; My commands -----------------------------
(leaf my-commands
  ;; compile key bind
  :bind (("C-x c" . compile))
  :config
  )

;; Change window previous
(global-set-key
 (kbd "C-x p")
 (lambda nil
   (interactive)
   (other-window -1)))

;; Settings to use only in Windows system. -----------------
(leaf settings-for-windows
  :when (or
	 (eq system-type 'windows-nt)
	 (eq system-type 'cygwin))
  :commands powershell
  :setq ((coq-prog-name . "C:/Program Files (x86)/Coq/bin/coqtop.exe")
	 (grep-use-null-device))
  :setq-default ((ispell-program-name . "~/.emacs.d/aspell.cmd"))
  :config
  (unless (cl-member 'cp65001 coding-system-list)
    (define-coding-system-alias 'cp65001 'utf-8))
  (setq shell-file-name (executable-find "bash"))
  (with-eval-after-load '"ispell"
    (add-to-list 'ispell-skip-region-alist
		 '("[^ -\377]+")))

  (set-face-attribute 'default nil :family "Roboto Mono" :height 125)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   (font-spec :family "Yu Gothic" :size 14)))

;; Settings to use only in Linux system. -------------------
(leaf settings-for-linux
  :when (eq system-type 'gnu/linux)
  :init
  (set-face-attribute 'default nil :family "SourceHanSansHW" :height 120)
  (set-fontset-font
   (frame-parameter nil 'font)
   'unicode
   (font-spec :family "SourceHanSansHW" :size 17))
  (with-eval-after-load '"ispell"
    (add-to-list 'ispell-skip-region-alist
		 '("[^ -\377]+")))

  :require mozc
  :setq ((default-input-method . "japanese-mozc"))
  :setq-default ((ispell-program-name . "aspell"))
  :config
  (add-hook 'input-method-activate-hook
	    (lambda nil
	      (set-cursor-color "red")))
  (add-hook 'input-method-inactivate-hook
	    (lambda nil
	      (set-cursor-color "black"))))

;;  Settings for screen----------------
(leaf settings-for-screen
  :setq ((show-paren-style quote mixed))
  :config
  (set-face-background 'mode-line-inactive "gray60")
  (global-hl-line-mode t)
  (set-face-background 'hl-line "lavender")
  (show-paren-mode t)
  (set-face-attribute 'show-paren-match nil :background "medium spring green")
  (transient-mark-mode t)
  (set-face-background 'region "Orange")
  ;; Default screen size 
  (add-to-list 'default-frame-alist
	       '(fullscreen . maximized))
  )

;; volatile-highlights ----------------
(leaf volatile-highlights
  :ensure volatile-highlights
  :config
  (volatile-highlights-mode t))

;;elscreen -----------------------
(leaf elscreen
  :ensure elscreen
  :bind (("<f9>" . elscreen-toggle))
  :config
  (setq elscreen-prefix-key (kbd "C-!"))
  (elscreen-start))

;; Settings for git -------------------
;; magit-status key bind
(leaf magit
  :ensure magit
  :bind (("C-x g" . magit-status)))
;; To use git-gutter
(leaf git-gutter
  ;; :when (require 'git-gutter nil t)
  :ensure git-gutter
  :config
  (global-git-gutter-mode t))

;; Settings for auto-complete ----------------
(leaf auto-complete
  :ensure auto-complete
  :when (require 'auto-complete-config nil t)
  :setq ((ac-use-menu-map . t)
	 (ac-use-fuzzy . t)
	 (ac-ignore-case))
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-set-trigger-key "TAB"))

;; Hokan
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode
  )

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

;; YaTeX-mode ---------------------
(leaf YaTeX
  :ensure yatex
  :commands yatex-mode
  :setq (
	 (tex-command . "lualatex")
	 (makeindex-command . "mendex")
	 (YaTeX-kanji-code . 4)
	 (YaTeX-need-nonstop . t)
	 )
  :config
  (setq auto-mode-alist (cons
			 (cons ".tex$" 'yatex-mode)
			 auto-mode-alist))
  (setq auto-mode-alist (cons
			 (cons ".ltx$" 'yatex-mode)
			 auto-mode-alist))
  (setq auto-mode-alist (cons
			 (cons ".sty$" 'yatex-mode)
			 auto-mode-alist))
  ;; yatex-mode-hook 
  (add-hook 'yatex-mode-hook
	    (lambda nil
	      (outline-minor-mode t)
	      (setq LaTeX-outline-regexp (concat "[ t]*"
						 (regexp-quote "")
						 "(appendix|documentstyle|part|chapter|" "section|subsection|subsubsection|" "paragraph|subparagraph)" "*?[ t]*[[{]"))
	      (make-local-variable 'outline-regexp)))
  ;;; YaHTML-mode
  (leaf YaHTML
    :commands yahtml-mode
    :config
    (setq auto-mode-alist (cons
			   (cons ".html$" 'yahtml-mode)
			   auto-mode-alist))
    )
  )

;;; BibTeX-mode ---------------------------------------
(leaf BibTeX
  :setq (
	 (bibtex-user-optional-fields quote
				      (("annote" "Personal annotation (ignored)")
				       ("yomi" "Yomigana")
				       ("location" "where it is (ignored)")
				       ("memo" "Memorundum (ignored)")))
	 )
  )

;; Settings for SATySFi ----------------
(leaf SATySFi
  :el-get gfngfn/satysfi.el
  :mode (("\\.saty$" . satysfi-mode)
	 ("\\.satyh$" . satysfi-mode))
  :require satysfi
  :setq (
	 ;; set the command for typesetting (default: "satysfi -b")
	 (satysfi-command . "satysfi")
	 )
  ;; set the command for opening PDF files (default: "open")
  :when (or
	 (eq system-type 'windows-nt)
	 (eq system-type 'cygwin))
  :setq (
	 (satysfi-pdf-viewer-command . "sumatrapdf")
	 )
  :when (eq system-type 'gnu/linux)
  :setq (
	 (satysfi-pdf-viewer-command . "evince")
	 )  
  )

;; ;; Settings for Coq
(leaf coq
  :ensure proof-general
  :commands coq-mode
  :config
  (setq auto-mode-alist (cons
			 '("\\.v$" . coq-mode)
			 auto-mode-alist))
  )

;;; flyspell-mode -------------
(leaf flyspell-mode
  :setq ((ispell-list-command . "--list"))
  :config
  (mapc
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '( ;; flyspell-mode for comments
     c-mode-common-hook emacs-lisp-mode-hook sh-mode-hook
			)
   )
  (mapc
   (lambda (hook)
     (add-hook hook
	       (lambda nil
		 (flyspell-mode 1))))
   '( ;; flyspell-mode 
     yatex-mode-hook bibtex-mode-hook git-commit-setup-hook git-commit-turn-on-flyspell
		     org-mode-hook
		     )
   ))

;; flycheck
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode
  )

;; evil-mode ----------------------------
(leaf evil
  :ensure evil
  :bind (("C-M-?" . evil-mode))
  )

;; Settings for Media Wiki
(leaf mediawiki
  :ensure mediawiki
  :bind ((mediawiki-mode-map
	  ("C-x C-s"))
	 (mediawiki-mode-map
	  ("C-x C-s" . save-buffer)))
  :setq ((browse-url-browser-function quote browse-url-generic))
  :config
  (setq auto-mode-alist (cons
			 (cons ".mediawiki$" 'mediawiki-mode)
			 auto-mode-alist))
  (setq mediawiki-mode-hook (lambda nil
			      (turn-off-auto-fill)))
  (setq browse-url-generic-program (executable-find "google-chrome"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
