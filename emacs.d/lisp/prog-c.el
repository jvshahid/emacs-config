;;; -*- lexical-binding: t; -*-

(defun c-c++-hook ()
  (eglot-ensure))

(defun shahid/c-key-bindings ()
  (lambda ()
    (local-set-key (kbd "C-c C-c") 'compile)
    (local-set-key (kbd "C-c C-m") 'man)))

(add-hook 'c-mode-hook #'shahid/c-key-bindings)

(add-hook 'c-mode-hook 'c-c++-hook)
(add-hook 'c++-mode-hook 'c-c++-hook)
