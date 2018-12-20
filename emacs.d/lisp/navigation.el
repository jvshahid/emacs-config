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

(defun define-repeatable-key (keyseq key cmd)
  "define a repeatable key sequence. KEYSEQ is the initial key
sequence invoking the command. KEY is the key sequence to invoke
the command again. CMD is the command to run"
  (global-set-key keyseq
                  (lambda ()
                    (interactive)
                    (funcall cmd)
                    (set-transient-map
                     (let ((map (make-sparse-keymap)))
                       (define-key map key 'repeat)
                       map)))))

(define-repeatable-key (kbd "C-c M-=") (kbd "M-=") (lambda () (enlarge-window 4)))
(define-repeatable-key (kbd "C-c M--") (kbd "M--") (lambda () (shrink-window 4)))
(define-repeatable-key (kbd "C-c M-.") (kbd "M-.") (lambda () (enlarge-window-horizontally 4)))
(define-repeatable-key (kbd "C-c M-,") (kbd "M-,") (lambda () (shrink-window-horizontally 4)))

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

