;;; -*- lexical-binding: t; -*-

(straight-use-package 'evil)
(straight-use-package 'evil-surround)

(with-eval-after-load 'evil
  (add-to-list 'evil-emacs-state-modes 'exwm-mode))

(evil-mode)
(global-undo-tree-mode -1)
(global-evil-surround-mode)
