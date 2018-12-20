(straight-use-package 'helm)
(straight-use-package '(helm-fzf :type git :host github :repo "ibmandura/helm-fzf"))

(global-set-key (kbd "C-c =") #'helm-show-kill-ring)
(global-set-key (kbd "M-x") #'helm-M-x)
(setq helm-locate-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)

;; stop using a new frame for helm completion
(setq helm-show-completion-display-function nil)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))

(setq-default helm-fzf--no-sort nil)
(autoload 'helm-fzf-project-root "helm-fzf" "Create a new commit on `HEAD' using `ci'.
With a prefix argument, amend to the commit at `HEAD' instead.
\n(git commit [--amend] ARGS)")

;;; override the default fzf find command
(global-set-key (kbd "C-x C-p") (lambda (arg)
                                  (interactive "P")
                                  (call-interactively (if arg
                                                          'helm-fzf
                                                        'helm-fzf-project-root))))
