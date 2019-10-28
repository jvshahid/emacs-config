;;; -*- lexical-binding: t; -*-

(straight-use-package 'multiple-cursors)

(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc/unsupported-minor-modes 'parinfer-mode)
  (require 'multiple-cursors))

(defhydra hydra-multiple-cursor (global-map "C-c ,")
  ("a" mc/mark-all-dwim)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("p" mc/unmark-next-like-this)
  ("e" mc/edit-lines))

(define-key mc/keymap (kbd "C-c '") 'mc-hide-unmatched-lines-mode)

(global-set-key (kbd "C-c ,") #'multiple-cursor-activate)
