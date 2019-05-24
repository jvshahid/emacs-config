;;; -*- lexical-binding: t; -*-

(straight-use-package 'multiple-cursors)

(defun multiple-cursor-activate ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'mc/mark-all-dwim)
    (define-key map (kbd "n") 'mc/mark-next-like-this)
    (define-key map (kbd "N") 'mc/skip-to-next-like-this)
    (define-key map (kbd "p") 'mc/unmark-next-like-this)
    (define-key map (kbd "e") 'mc/edit-lines)
    (set-transient-map map t)))

(add-to-list 'mc/unsupported-minor-modes 'parinfer-mode)

(global-set-key (kbd "C-c ,") #'multiple-cursor-activate)
