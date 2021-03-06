;;; -*- lexical-binding: t; -*-

(straight-use-package 'ghub :files '("*.el"))
(straight-use-package 'magit)
(straight-use-package 'forge)
(straight-use-package 'git-link)

(with-eval-after-load 'git-link
  (setq git-link-use-commit t))

(define-key global-map (kbd "C-x g") 'magit-status)

(add-hook 'git-commit-mode-hook #'flyspell-mode)

(with-eval-after-load 'magit
  (require 'forge)
  (add-hook 'magit-mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))
  (setq
   auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p))

(autoload 'magit-toplevel "magit-git")
;; work around an issue with magit
(autoload 'magit-process-file "magit-process")

(add-to-list 'display-buffer-alist
             '("^magit: .*" (display-buffer-reuse-window
                             display-buffer-same-window)))

(defun magit-pullify ()
  (interactive)
  (magit-run-git "config" "--add" "remote.origin.fetch" "+refs/pull/*/head:refs/remotes/origin/pr/*")
  (magit-fetch-all nil))

(custom-set-variables '(auto-revert-use-notify nil))
