;;; -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'git-link)

(magit-auto-revert-mode)

(with-eval-after-load 'git-link
  (setq git-link-use-commit t))

(define-key global-map (kbd "C-x g") 'magit-status)

(add-hook 'git-commit-mode-hook #'flyspell-mode)

(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))
  (setq
   auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p))

(autoload 'magit-toplevel "magit-git")

(add-to-list 'display-buffer-alist
             '("^magit: .*" (display-buffer-reuse-window
                             display-buffer-same-window)))

(defun magit-pullify ()
  (interactive)
  (magit-run-git "config" "--add" "remote.origin.fetch" "+refs/pull/*/head:refs/remotes/origin/pr/*")
  (magit-fetch-all nil))
