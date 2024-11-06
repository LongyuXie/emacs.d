;;; init-editing.el --- init editing config -*- lexical-binding: t -*-

;; 设置默认编码为 UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(use-package smartparens-mode
  :straight smartparens  ;; install the package
  :diminish smartparens-mode
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(setq delete-trailing-lines nil)

;; (use-package whitespace
;;   :ensure nil
;;   :config
;;   (delete 'lines whitespace-style)
;;   (delete 'newline-mark whitespace-style)
;;   (bind-key* (kbd "<f7>") #'(lambda ()
;;                               (interactive)
;;                               (whitespace-mode 'toggle))))
;; edit surround pairs

;; (use-package rainbow-delimiters
;;   :hook (prog-mode text-mode markdown-mode))


(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))

;; (use-package 'whitespace-cleanup-mode
;;              :straight t)

;; (add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
;; (with-eval-after-load 'whitespace-cleanup-mode
;;   (diminish 'whitespace-cleanup-mode))
;;
;; (global-set-key [remap just-one-space] 'cycle-spacing)
(setq select-enable-clipboard t)


;; 自动加载外部修改过的文件
(auto-revert-mode 1)

;; 高亮配对括号
(show-paren-mode t)
(blink-cursor-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(defun split-line-into-sentences ()
  "Split the current line into sentences, each ending with a Chinese or English period, and retain indentation."
  (interactive)
  (save-excursion
    ;; 获取当前行的缩进
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line-text (buffer-substring-no-properties line-start line-end))
           (indentation (buffer-substring-no-properties line-start (progn (back-to-indentation) (point)))))
      ;; 删除当前行的文本
      (delete-region line-start line-end)
      ;; 按句号分割当前行的文本
      (dolist (sentence (split-string line-text "\\([。.]\\)" t "\\([。.]\\)"))
        ;; 插入缩进、句子和句号到新行
        (insert indentation (string-trim sentence) "。\n")))))

(provide 'init-editing)
