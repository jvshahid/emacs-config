;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))

(require-with-check 'project nil 'reload)
(require-with-check 'xref nil 'reload)
(straight-use-package 'eglot)

