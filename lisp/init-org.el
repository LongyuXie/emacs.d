;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.
;; Those other locations are generally other org files, which should

;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

;; (define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      ;; org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-odd-levels-only nil)
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)
(setq org-highlight-latex-and-related '(latex))
(setq org-src-fontify-natively t)

;;; Capturing
(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\n   %U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n  %U\n%a\n" :clock-resume t)
        ))


;;; Refiling
(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "TODO")



(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

;;; Ensure files and directories exist
(defun ensure-file-exists (file)
  "Create FILE if it doesn't exist."
  (unless (file-exists-p file)
    (make-empty-file file)))

(defun ensure-directory-exists (directory)
  "Create DIRECTORY if it doesn't exist."
  (unless (file-exists-p directory)
    (make-directory directory t))) ; t means create parent directories if needed

(defun ensure-org-directories ()
  "Ensure all required org directories exist."
  (let ((directories '("~/orgfiles"
                      "~/orgfiles/week"
                      "~/org-roam")))
    (dolist (dir directories)
      (ensure-directory-exists dir))))

;; Create required files and directories
(ensure-file-exists "~/inbox.org")
(ensure-org-directories)

(setq org-default-notes-file "~/inbox.org")
(setq org-agenda-files '("~/inbox.org" "~/orgfiles/"
			 "~/orgfiles/week"))


(defun org-insert-image ()
  (interactive)
  (let* ((path (concat default-directory "img/"))
         (image-file (concat
                     path
                     (buffer-name)
                     (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    ;; Handle different OS clipboard commands
    (condition-case err
        (progn
          (let ((exit-code
                 (cond
                  ((eq system-type 'gnu/linux)
                   (shell-command (concat "xclip -selection clipboard -t image/png -o >" image-file)))
                  ((eq system-type 'windows-nt)
                   (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;$img = [Windows.Forms.Clipboard]::GetImage();$img.Save('" image-file "');\"")))
                  (t (message "OS not supported for clipboard image paste") 1))))
            (when (= exit-code 0)
              (org-insert-link nil (concat "file:" image-file) "")
              (org-display-inline-images))))
      (error (message "Failed to insert image: %s" (error-message-string err))))))

;; org mode keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-c t") 'org-display-inline-images)
  (define-key org-mode-map (kbd "M-c p") 'Org-insert-image))



(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; ensure the week days are in English
(setq system-time-locale "C")
(setq org-time-stamp-formats '("<%Y-%m-%d %a %H:%M>" . "%Y-%m-%d %a %H:%M>"))


;; custom functions
(defun ly-inbox ()
  "Open the inbox file."
  (interactive)
  (find-file org-default-notes-file))

(provide 'init-org)
