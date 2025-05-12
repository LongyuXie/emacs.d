;;; init-blog.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq blog-project-dir "~/Sync/longyuxie.github.io")

(defun init-blog-project ()
  "初始化博客仓库目录。
  检查目标目录是否存在，如果不存在或为空目录，则从 GitHub 克隆仓库。
  仓库地址默认为 'https://github.com/longyuxie/longyuxie.github.io.git'。"
  (interactive)
  (let ((dir (expand-file-name blog-project-dir))
	(repo-url "https://github.com/longyuxie/longyuxie.github.io.git"))
    (if (file-exists-p dir)
	(if (directory-files dir nil "^[^.]" t)
	    (message "博客目录已存在且非空: %s" dir)
	  (when (yes-or-no-p (format "目录 %s 为空，是否从 GitHub 克隆仓库？" dir))
	    (message "正在克隆仓库到 %s..." dir)
	    (delete-directory dir)
	    (shell-command (format "git clone %s %s" repo-url dir))
	    (message "仓库克隆完成！")))
      (when (yes-or-no-p (format "目录 %s 不存在，是否创建并从 GitHub 克隆仓库？" dir))
	(make-directory dir t)
	(message "正在克隆仓库到 %s..." dir)
	(shell-command (format "git clone %s %s" repo-url dir))
	(message "仓库克隆完成！")))))

(defun create-new-blog ()
  "创建一个新的博客文章。
  在博客目录的 _posts 子目录下创建一个新的 Markdown 文件，
  文件名格式为 yyyy-mm-dd-title.md，并插入 Jekyll 格式的头部信息。
  创建后自动打开文件进行编辑。"
  (interactive)
  (let* ((posts-dir (expand-file-name "_posts" blog-project-dir))
	 (title (read-string "文章标题: "))
	 (slug (replace-regexp-in-string " " "-" (downcase title)))
	 (date (format-time-string "%Y-%m-%d"))
	 (time (format-time-string "%H:%M:%S +0800"))
	 (categories (read-string "分类标签 (用空格分隔多个标签): "))
	 (file-name (format "%s-%s.md" date slug))
	 (file-path (expand-file-name file-name posts-dir)))

    ;; 确保 _posts 目录存在
    (unless (file-exists-p posts-dir)
      (make-directory posts-dir t))

    ;; 创建文件并写入头部信息
    (with-temp-file file-path
      (insert "---\n")
      (insert (format "layout: post\n"))
      (insert (format "title:  \"%s\"\n" title))
      (insert (format "date:   %s %s\n" date time))
      (insert (format "categories: %s\n" categories))
      (insert "---\n\n"))

    ;; 打开文件进行编辑
    (find-file file-path)
    (message "已创建博客文章: %s" file-path)))

(provide 'init-blog)
;;; init-blog.el ends here
