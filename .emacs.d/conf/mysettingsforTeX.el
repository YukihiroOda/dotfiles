;;; YaTeX-mode ------------------------------
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq makeindex-command "mendex")
(setq auto-mode-alist (cons (cons ".tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons ".ltx$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons ".sty$" 'yatex-mode) auto-mode-alist))
(setq YaTeX-kanji-code 4) ;utf-8
(setq YaTeX-need-nonstop t)
;; yatex-mode-hook の設定
(add-hook 'yatex-mode-hook
	  (lambda () (outline-minor-mode t)
	    (setq LaTeX-outline-regexp
		  (concat "[ t]*" (regexp-quote "")
			  "(appendix|documentstyle|part|chapter|" "section|subsection|subsubsection|" "paragraph|subparagraph)" "*?[ t]*[[{]"))
	    (make-local-variable 'outline-regexp) ))
(setq dvi2-command "xdvi -geo +0+0 -s 6")

;;; AUCTeX-mode ------------------------------
(require 'tex-site)
(setq TeX-default-mode 'japanese-latex-mode)
(setq japanese-TeX-command-default "pTeX")
(setq japanese-LaTeX-command-default "pLaTeX")
(setq japanese-LaTeX-default-style "jsarticle")
(setq-default TeX-master nil)
(setq TeX-parse-self t)

;;; BibTeX-mode ---------------------------------------
(setq bibtex-user-optional-fields
      '(("annote" "Personal annotation (ignored)")
	("yomi" "Yomigana")
	("location" "where it is (ignored)")
	("memo" "Memorundum (ignored)")
	))
