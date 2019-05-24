;;; -*- lexical-binding: t; -*-

(straight-use-package 'multiple-cursors)

(defun multiple-cursor-activate ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'mc/mark-all-dwim)
    (define-key map (kbd "n") 'mc/mark-next-like-this)
    (define-key map (kbd "N") 'mc/skip-to-next-like-this)
    (define-key map (kbd "p") 'mc/unmark-next-like-this)
    (set-transient-map map t)))

(global-set-key (kbd "C-c ,") #'multiple-cursor-activate)
