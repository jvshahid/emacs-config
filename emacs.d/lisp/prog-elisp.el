;;; -*- lexical-binding: t; -*-

(straight-use-package 'paredit)
(straight-use-package '(parinfer :type git
                                 :host github
                                 :repo  "DogLooksGood/parinfer-mode"
                                 :branch "master"))
(with-eval-after-load 'parinfer
  (require 'paredit)
  (setq parinfer-extensions
        '(defaults
           pretty-parens
           smart-tab
           paredit)))

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

