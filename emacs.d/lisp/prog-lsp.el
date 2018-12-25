;;; -*- lexical-binding: t; -*-

(straight-use-package 'eglot)
(straight-use-package 'company)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))
