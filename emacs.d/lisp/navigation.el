;;; -*- lexical-binding: t; -*-

(straight-use-package '(emacs-rotate :repo "daichirata/emacs-rotate" :host github))
(straight-use-package 'golden-ratio)

(golden-ratio-mode)
(add-to-list 'golden-ratio-extra-commands 'magit-show-commit)
(add-to-list 'golden-ratio-extra-commands 'calendar)
(add-to-list 'golden-ratio-extra-commands 'calc)
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'find-file-at-point)

(global-set-key (kbd "C-c 1") #'rotate:even-vertical)
(global-set-key (kbd "C-c 2") #'rotate:even-horizontal)
(global-set-key (kbd "C-c 3") #'rotate:main-vertical)
(global-set-key (kbd "C-c 4") #'rotate:main-horizontal)
(global-set-key (kbd "C-c 5") #'rotate:tiled)

(autoload 'aw-swap-window "ace-window")

(defun swap-next-window (n)
  "swap the buffer of the current window with the next window obtained using 'next-window"
  (interactive "p")
  (let ((fun  (if (< n 0) 'previous-window 'next-window))
        (n    (abs n)))
    (dotimes (_ n)
      (aw-swap-window (funcall fun)))))

(defun swap-previous-window (n)
  "swap the buffer of the current window with the previous window obtained using 'previous-window"
  (interactive "p")
  (swap-next-window (- n)))

(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c ]") 'swap-next-window)
(global-set-key (kbd "C-c [") 'swap-previous-window)

(defun shahid/resize-window (inc)
  "Adjust the dimensions of window by INC.

INC may be passed as a numeric prefix argument.

The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers
removed:

    >,. Enlarges the current window horizontally
    <,, Shrinks the current window horizontally
    =   Enlarges the current window vertically
    -   Shrinks the current window vertically

After adjusting, continue to read input events and further adjust
the dimenstions, where the event is one of the above characters.
"
  (interactive "p")
  (let ((event (event-basic-type last-input-event)))
    (cond
     ((= event ?=) (enlarge-window inc))
     ((= event ?-) (shrink-window inc))
     ((= event ?.) (enlarge-window-horizontally inc))
     ((= event ?,) (shrink-window-horizontally inc)))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (key '("=" "-" "." ","))
         (define-key
           map
           (kbd (concat "M-" key))
           (lambda () (interactive) (shahid/resize-window inc))))
       map))))

(dolist (key '("=" "-" "." ","))
  (global-set-key (kbd (concat "C-c M-" key)) 'shahid/resize-window))

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

