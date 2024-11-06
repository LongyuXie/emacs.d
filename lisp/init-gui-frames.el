;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(global-display-line-numbers-mode)
;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(when (font-installed-p "Sarasa Term SC Nerd")
  (set-face-attribute 'default nil
		      :font (font-spec
			      :family "Sarasa Term SC Nerd"
			      :weight 'normal
			      :slant 'normal
			      :size 16.0
			      ))
  (set-fontset-font t 'han
                    (font-spec
		     :name "Sarasa Term SC Nerd"
		     :weight 'normal
		     :slant 'normal
		     :size 16.0)))


;; (when (display-graphic-p)
;;   (cl-loop for font in '("CaskaydiaCove NFM" "SF Mono" "Source Code Pro"
;;                          "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
;;                          "Lucida Console" "Consolas" "SAS Monospace")
;;            when (font-installed-p font)
;;            return (set-face-attribute
;;                    'default nil
;;                    :font (font-spec :family font
;;                                     :weight 'normal
;;                                     :slant 'normal
;;                                     :size (cond ((eq system-type 'gnu/linux) 16.0)
;;                                                 ((eq system-type 'windows-nt) 16.0)))))
;;   (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
;;                          "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
;;            when (font-installed-p font)
;;            return (set-fontset-font t 'unicode
;;                                     (font-spec :family font
;;                                                :size (cond ((eq system-type 'gnu/linux) 16.5)
;;                                                            ((eq system-type 'windows-nt) 16.0)))
;;                                     nil 'prepend))
;;   (cl-loop for font in '("Sarasa Term SC Nerd" "霞鹜文楷等宽" "Microsoft Yahei" "微软雅黑 CN"
;;                          "Source Han Sans CN" "Source Han Serif CN"
;;                          "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
;;                          "Microsoft Yahei UI" )
;;            when (font-installed-p font)
;;            return (set-fontset-font t '(#x4e00 . #x9fff)
;;                                     (font-spec :name font
;;                                                :weight 'normal
;;                                                :slant 'normal
;;                                                :size (cond ((eq system-type 'gnu/linux) 16.5)
;;                                                            ((eq system-type 'windows-nt) 16.0)))))
;;   (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
;;            when (font-installed-p font)
;;            return (set-fontset-font t '(#x20000 . #x2A6DF)
;;                                     (font-spec :name font
;;                                                :weight 'normal
;;                                                :slant 'normal
;;                                                :size (cond ((eq system-type 'gnu/linux) 16.5)
;;                                                            ((eq system-type 'windows-nt) 16.0))))))

(setq visible-bell t)
(setq ring-bell-function 'ignore)


;; Window size and features

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
