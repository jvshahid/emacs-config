;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))

(require-with-check 'project nil 'reload)
(require-with-check 'xref nil 'reload)
(straight-use-package 'eglot)

(defun my-eglot-hook ()
  (local-unset-key (kbd "C-c C-j"))
  (local-set-key (kbd "C-c C-j a") #'eglot-code-actions)
  (local-set-key (kbd "C-c C-j f") #'eglot-format)
  (local-set-key (kbd "C-c C-j i") #'eglot-find-implementation)
  (local-set-key (kbd "C-c C-j C") #'eglot-show-call-hierarchy)
  (local-set-key (kbd "C-c C-j T") #'eglot-show-type-hierarchy)
  (local-set-key (kbd "C-c C-j r") #'eglot-rename))

(add-hook 'eglot-managed-mode-hook #'my-eglot-hook)

;; Install eglot-format-buffer as a save hook.  The depth of -10
;; places this before eglot's willSave notification, so that that notification
;; reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))


;; remove '(left-fringe margin) from code indications. They tend to shift the
;; layout of the page and mess things up visually. I don't like them.
(setq eglot-code-action-indications '(eldoc-hint))
