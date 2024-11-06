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

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(setq straight-use-package-by-default t)

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

;; 设置默认编码为 UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; 自动加载外部修改过的文件
(auto-revert-mode 1)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-utils)
(require 'init-editing)
(require 'init-org)
(require 'init-evil)
(require 'init-programming)
(require 'init-completion)
(require 'init-consult)
(require 'init-project)


(provide 'init)
