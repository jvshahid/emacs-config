;;; -*- lexical-binding: t; -*-

(straight-use-package 'racket-mode)

(add-hook 'racket-mode-hook #'parinfer-mode)

;; Don't use racket-smart-open-bracket since it interferes with parinfer
(with-eval-after-load 'racket-mode
	(define-key racket-mode-map (kbd "[") #'self-insert-command))
