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

(defhydra windows-hydra (global-map "C-c m")
  ("=" balance-windows)
  ("v" enlarge-window)
  ("V" shrink-window)
  ("h" enlarge-window-horizontally)
  ("H" shrink-window-horizontally)
  ("]" swap-next-window)
  ("[" swap-previous-window)
  ("!" rotate:even-vertical)
  ("@" rotate:even-horizontal)
  ("#" rotate:main-vertical)
  ("$" rotate:main-horizontal)
  ("%" rotate:tiled)
  ("u" winner-undo)
  ("r" winner-redo))

(defhydra navigation-hydra (global-map "C-c n")
  ("p" previous-line)
  ("n" next-line)
  ("f" forward-sexp)
  ("b" backward-sexp)
  ("a" beginning-of-sentence)
  ("e" end-of-sentence)
  ("u" backward-up-list)
  ("SPC" mark-sexp)
  ("," beginning-of-buffer)
  ("." end-of-buffer)
  ("]" forward-paragraph)
  ("[" backward-paragraph))

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

