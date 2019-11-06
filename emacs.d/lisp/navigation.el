;;; -*- lexical-binding: t; -*-

(straight-use-package '(emacs-rotate :repo "daichirata/emacs-rotate" :host github))
(straight-use-package 'expand-region)

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

(defhydra windows-hydra (global-map "C-c w")
  ("=" balance-windows)
  ("v" enlarge-window)
  ("V" shrink-window)
  ("h" enlarge-window-horizontally)
  ("H" shrink-window-horizontally)
  ("]" swap-next-window)
  ("[" swap-previous-window)
  ("1" rotate:even-vertical)
  ("2" rotate:even-horizontal)
  ("3" rotate:main-vertical)
  ("4" rotate:main-horizontal)
  ("5" rotate:tiled)
  ("u" winner-undo)
  ("r" winner-redo))

(defun shahid/mark-sexp-inner ()
  (interactive)
  (mark-sexp)
  (forward-char)
  (skip-chars-forward "[:blank:]\\|[:cntrl:]")
  (exchange-point-and-mark)
  (backward-char)
  (skip-chars-backward "[:blank:]\\|[:cntrl:]")
  (exchange-point-and-mark))

(defhydra navigation-hydra (global-map "C-c n")
  ("f" forward-sexp)
  ("b" backward-sexp)
  ("a" beginning-of-defun)
  ("e" end-of-defun)
  ("h" mark-defun)
  ("u" backward-up-list)
  ("d" down-list)
  ("SPC" mark-sexp)
  ("M-SPC" shahid/mark-sexp-inner)
  ("k" kill-sexp)
  ("," beginning-of-buffer)
  ("." end-of-buffer)
  ("]" forward-paragraph)
  ("[" backward-paragraph))

(global-set-key (kbd "C-c n r") #'er/expand-region)

(defhydra hide-show-hydra (global-map "C-c t")
  ("l" hs-hide-level)
  ("s" hs-show-block)
  ("h" hs-hide-block)
  ("S" hs-show-all)
  ("H" hs-hide-all))

(defun shahid/indent-rigidly-extend-region (&rest _)
  (exchange-point-and-mark)
  (beginning-of-line)
  (exchange-point-and-mark))

(advice-add 'indent-rigidly :before #'shahid/indent-rigidly-extend-region)

(define-key indent-rigidly-map (kbd ",") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd ".") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "<") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd ">") 'indent-rigidly-right-to-tab-stop)

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

