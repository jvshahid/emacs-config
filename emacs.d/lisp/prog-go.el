;; -*- lexical-binding: t; -*-

(straight-use-package 'go-mode)

;; disable the use of CGO inside emacs. otherwise, it complains that gcc is
;; missing while flychecking a buffer
(setenv "CGO_ENABLED" "1")

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-x 4 .") 'godef-jump-other-window)

  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'hs-minor-mode)
  (add-hook 'go-mode-hook 'eglot-ensure))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; Optional: install eglot-format-buffer as a save hook.  The depth of -10
;; places this before eglot's willSave notification, so that that notification
;; reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

(setenv "PATH" (concat (getenv "PATH") ":/Users/jvshahid/homebrew/bin/"))

