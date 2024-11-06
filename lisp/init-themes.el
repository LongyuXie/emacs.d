;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; (use-package color-theme-sanityinc-solarized)
;; (use-package color-theme-sanityinc-tomorrow)
;; If you don't customize it, this is the theme you get.
(straight-use-package 'zenburn-theme)
(straight-use-package 'dracula-theme)
(straight-use-package 'material-theme)
(straight-use-package 'monokai-theme)
(straight-use-package 'molokai-theme)
(straight-use-package 'darcula-theme)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)
(setq-default custom-enabled-themes '(zenburn))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(provide 'init-themes)
;;; init-themes.el ends here
