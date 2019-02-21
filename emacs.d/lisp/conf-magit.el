;;; -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'git-link)

(with-eval-after-load 'git-link
  (setq git-link-use-commit t))

(define-key global-map (kbd "C-x g") 'magit-status)
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))
  (setq
   auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)

  (defun shahid/magit-replace-command (args)
    (message "called with: %S " args)
    (pcase-let ((`(,cmd . ,args) args))
      (cond
       ((string-equal cmd "commit") (cons "ci" args))
       (t (cons cmd args)))))

  (advice-add #'magit-run-git-with-editor
              :filter-args
              #'shahid/magit-replace-command))
