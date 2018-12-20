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
  (magit-define-popup-action 'magit-commit-popup ?i "Commit using ci" 'magit-ci-create ?c t))

(defun magit-ci-create (&optional args)
  "Create a new commit on `HEAD' using `ci'.
With a prefix argument, amend to the commit at `HEAD' instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (magit-commit-arguments)))
                 (list (magit-commit-arguments))))
  (when (member "--all" args)
    (setq this-command 'magit-commit-all))
  (when (setq args (magit-commit-assert args))
    (let ((default-directory (magit-toplevel)))
      (magit-run-git-with-editor "ci" args))))

