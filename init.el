(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugin" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(use-package benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(when (eq system-type 'windows-nt)
  (setq default-directory "C:/Users/xielongyu/")
)

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))

(setq process-adaptive-read-buffering nil)
(use-package diminish :straight t)

(use-package gcmh :straight t
             :init
             (setq gcmh-high-cons-threshold (* 128 1024 1024))
             :hook
             (after-init-hook . (lambda ()
                                (gcmh-mode)
                                (diminish 'gcmh-mode))))
(setq jit-lock-defer-time 0)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-utils)
(require 'init-editing)
(require 'init-org)
(require 'init-evil)
;; (require 'init-programming)
(require 'init-completion)
;; (require 'init-consult)
;; (require 'init-project)
;; (require 'init-latex)
;; (require 'init-git)


;; 在你的init.el文件最开始添加




;; (use-package request)
;; (require 'lyblog)
;; (require 'lycard)
;; (require 'leetcode)

;; 设置窗口分割偏好为垂直分割（竖屏）
(setq split-height-threshold nil)  ;; 不根据高度自动分割
(setq split-width-threshold 0)     ;; 总是优先水平分割（产生上下窗口）


(provide 'init)
