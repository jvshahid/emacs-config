;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'eglot-ensure))

(add-hook 'rust-mode-hook #'eglot-format-buffer-on-save)
