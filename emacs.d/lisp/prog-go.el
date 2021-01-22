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

