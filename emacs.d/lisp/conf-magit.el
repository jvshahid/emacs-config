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
    "Replaces git first arguments.

To work around the limitation that magit doesn't have
customizable variables for the commands to use.  Currently this
function only replaces `commit' with `ci' which is the my local
alias for git-duet-commit."
    (pcase args
      (`("commit" . ,args) (cons "ci" args))
      (_ args)))

  (advice-add #'magit-run-git-with-editor
              :filter-args
              #'shahid/magit-replace-command))
