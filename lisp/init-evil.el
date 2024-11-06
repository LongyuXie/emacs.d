(use-package undo-fu :straight t)
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :bind
  (
    :map evil-insert-state-map
    ("C-p" . nil)
    ("C-n" . nil)
  )
  :config
  
  (evil-set-leader nil (kbd "SPC"))
  (setq evil-disable-insert-state-bindings t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(define-key evil-commentary-mode-map
  (kbd "C-/") 'evil-commentary-line)


;; (defun my/simulate-ctrl-space ()
;;   "Simulate pressing Ctrl+Space."
;;   (interactive)
;;   (shell-command "C:/Bin/SimulateCtrlSpace.exe"))

(defun my/set-ime-to-english ()
  "Set IME to English (disable IME)."
  (cond ((eq system-type 'windows-nt)
         ;; TODO: check if rime is current input method
         ;;  win11 rime status is opposite to other input method, e.g. sougou, microsoft pinyin
         (w32-set-ime-open-status t)
         (message "IME disabled")
         )
        ((eq system-type 'gnu/linux)
         (shell-command "fcitx5-remote -c"))
        (t nil)))

(add-hook 'evil-insert-state-exit-hook #'my/set-ime-to-english)



(provide 'init-evil)
