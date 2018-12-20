;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         formatting functions        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-symbol-value (&rest names)
  "return the value of the variable whose name is given by concatenating
   all the given arguments or nil if the variable isn't set."
  (let ((s (intern (apply 'concat names))))
    (if (boundp s)
        (eval s))))

(defun fmt-before-save ()
  "Add this to .emacs to format the current buffer when saving:
 (add-hook 'before-save-hook 'fmt-before-save)."

  (interactive)
  (let* ((mode (cl-first (split-string (symbol-name major-mode) "-")))
         (fmt-command (get-symbol-value mode "fmt-command"))
         (fmt-args (get-symbol-value mode "fmt-args")))
    (if fmt-command
        (fmt fmt-command fmt-args))))

(add-hook 'before-save-hook #'fmt-before-save)

(defun fmt (fmt-command fmt-args)
  (save-restriction
    (widen)
    (let ((buf (get-buffer-create "*patch*")))
      (with-current-buffer buf (erase-buffer))
      (if (/= 0 (apply 'call-process-region nil nil fmt-command nil buf nil fmt-args))
          (signal "formatting failed"))
      (replace-buffer-contents buf))))
