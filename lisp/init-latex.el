(use-package tex
  :straight auctex)

(setq-default TeX-master nil) ; Query for master file.


;; 定义 LaTeX 模式在哪些环境下启用
(mapc (lambda (mode)
	(add-hook 'LaTeX-mode-hook mode))
      (list 'LaTeX-math-mode
            'turn-on-reftex))

;; 作 LaTeX 相关设定
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; 在保存的时候自动去掉的 TAB 空白
                  TeX-engine 'xetex       ; 默认使用 XeLaTeX 编译引擎
                  TeX-show-compilation t) ; 显示编译输出窗口
            (TeX-global-PDF-mode t)       ; 启用 PDF 模式
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

;; 如果 cdlatex 没有安装，则进行安装
;; (install-package-if-not-found 'cdlatex)

;; 编译工具链 xelatex -> bibtex -> xelatex -> xelatex
(add-to-list 'TeX-command-list '("xe-bib-xe2" "xelatex -interaction=nonstopmode -synctex=1 %s && bibtex %s && xelatex -interaction=nonstopmode -synctex=1 %s && xelatex -interaction=nonstopmode -synctex=1 %s" TeX-run-command nil t :help "Run XeLaTeX and BibTeX"))
;; pdflatex -> bibtex -> pdflatex -> pdflatex
(add-to-list 'TeX-command-list '("pdf-bib-pdf2" "pdflatex -interaction=nonstopmode -synctex=1 %s && bibtex %s && pdflatex -interaction=nonstopmode -synctex=1 %s && pdflatex -interaction=nonstopmode -synctex=1 %s" TeX-run-command t t :help "Run PDFLaTeX and BibTeX"))
;; 在 LaTeX 模式被启用时，启用 cdlatex
(use-package cdlatex
  :straight t
  :config
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

;; 提供 init-cdlatex 特性.

(provide 'init-latex)
