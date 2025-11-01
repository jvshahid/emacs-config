;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'subword-mode)
  (add-hook 'rust-mode-hook 'yas-minor-mode)
  (add-hook 'rust-mode-hook 'hs-minor-mode)
  (add-hook 'rust-mode-hook 'eglot-ensure))

;; Optional: install eglot-format-buffer as a save hook.  The depth of -10
;; places this before eglot's willSave notification, so that that notification
;; reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'rust-mode-hook #'eglot-format-buffer-on-save)
