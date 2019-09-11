;;; -*- lexical-binding: t; -*-

(straight-use-package '(emacs-rotate :repo "daichirata/emacs-rotate" :host github))

(straight-use-package 'ace-window)
(autoload 'aw-swap-window "ace-window")

(defun swap-next-window (n)
  "swap the buffer of the current window with the next window obtained using 'next-window"
  (interactive "p")
  (let ((win  (if (< n 0)
                  (previous-window)
                (next-window))))
    (aw-swap-window win)))

(defun swap-previous-window ()
  "swap the buffer of the current window with the previous window obtained using 'previous-window"
  (interactive)
  (swap-next-window (- 1)))

(defun shahid/windows-mode ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (set-transient-map map t)
    (define-key map (kbd "-") #'shrink-window)
    (define-key map (kbd "=") #'enlarge-window)
    (define-key map (kbd ".") #'enlarge-window-horizontally)
    (define-key map (kbd ",") #'shrink-window-horizontally)
    (define-key map (kbd "]") #'swap-next-window)
    (define-key map (kbd "[") #'swap-previous-window)
    (define-key map (kbd "1") #'rotate:even-vertical)
    (define-key map (kbd "2") #'rotate:even-horizontal)
    (define-key map (kbd "3") #'rotate:main-vertical)
    (define-key map (kbd "4") #'rotate:main-horizontal)
    (define-key map (kbd "5") #'rotate:tiled)))

(global-set-key (kbd "C-c m") #'shahid/windows-mode)

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

