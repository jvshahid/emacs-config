;; -*- lexical-binding: t; -*-

(straight-use-package 'go-mode)

;; disable the use of CGO inside emacs. otherwise, it complains that gcc is
;; missing while flychecking a buffer
(setenv "CGO_ENABLED" "1")

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-x 4 .") 'godef-jump-other-window)

  (add-hook 'go-mode-hook 'eglot-ensure))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

(with-eval-after-load 'eglot
  (setq eglot-workspace-configuration
        (plist-put eglot-workspace-configuration
                   :gopls
                   '((staticcheck . nil)
                     (matcher . "CaseSensitive")))))

