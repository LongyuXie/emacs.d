
(use-package projectile)
;; (setq projectile-project-search-path '("~/workspace/"))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package neotree
  :straight t
  :config
  (global-set-key [f8] 'neotree-toggle))


(projectile-mode +1)

(use-package consult-projectile
  :after (consult projectile)
  :bind
  ("C-c p f" . consult-projectile)
  ("C-c p b" . consult-projectile-switch-to-buffer))

(provide 'init-project)
