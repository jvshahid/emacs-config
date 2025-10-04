;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))

(require-with-check 'project nil 'reload)
(require-with-check 'xref nil 'reload)
(straight-use-package 'eglot)

(defun my-eglot-hook ()
  (local-set-key (kbd "C-c C-j a") #'eglot-code-actions)
  (local-set-key (kbd "C-c C-j f") #'eglot-format)
  (local-set-key (kbd "C-c C-j r") #'eglot-rename))

(add-hook 'eglot-managed-mode-hook #'my-eglot-hook)

