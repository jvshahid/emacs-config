;;; -*- lexical-binding: t; -*-

(straight-use-package 'paredit)
(straight-use-package 'selected)
(straight-use-package '(parinfer :type git
                                 :host github
                                 :repo  "DogLooksGood/parinfer-mode"
                                 :branch "smart"))
(with-eval-after-load 'parinfer
  (put 'parinfer-smart-tab:forward-char 'isearch-move 'enabled)
  (put 'parinfer-smart-tab:backward-char 'isearch-move 'enabled)
  (require 'paredit)
  (setq parinfer-extensions
        '(defaults
           pretty-parens
           smart-tab
           smart-yank
           paredit))
  (define-key parinfer-mode-map (kbd "C-x p") 'parinfer-toggle-mode))

(add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-x p") 'parinfer-toggle-mode)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun align-elisp-let (begin end)
  (interactive "r")
  (align-regexp begin
                end
                "[a-z]\\(\\s-+\\)\\((\\|[a-z]\\)"))

