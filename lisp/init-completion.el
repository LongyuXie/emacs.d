(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  ;; (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  :bind
  (
   :map corfu-map
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   )
  :init
  (global-corfu-mode)
  )

;; (define-prefix-command 'my-corfu-cape-map)
;; (global-set-key (kbd "A-c") 'my-corfu-cape-map)
(setq cape-dict-file "~/.emacs.d/dict/en_cn.txt")
;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  :bind (
	 ;; ("A-c h" . cape-history)
	 ;; ("A-c f" . cape-file)
	 ;; ("M-p d" . cape-dict)
	 ;; ("C-c p d" . cape-dabbrev)
	 )
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))

(use-package yasnippet)
(use-package yasnippet-snippets)


(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; (define-key corfu-map [tab] nil)
;; (define-key corfu-map "\t" nil)

(use-package jsonrpc)
(use-package editorconfig)
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (
	 (text-mode . copilot-mode)
	 (prog-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4)))

(provide 'init-completion)

