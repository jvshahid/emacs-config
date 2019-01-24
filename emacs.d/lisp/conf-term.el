;;; -*- lexical-binding: t; -*-

;; configuration for term and eshell

(require 'cl)

(setq term-buffer-maximum-size 0)
(defun rebind-window-text-height (orig-func &rest args)
  (flet ((window-text-height () (floor (window-screen-lines))))
    (apply orig-func args)))
(advice-add 'term-mode :around #'rebind-window-text-height)
(add-hook 'term-mode-hook (lambda ()
                            ;; fix C-c non-prefix error when eshell runs visual commands
                            (term-set-escape-char ?\C-c)
                            (define-key term-raw-map (kbd "C-/") (lambda ()
                                                                   (interactive)
                                                                   (term-send-raw-string (kbd "C-_"))))
                            (define-key term-raw-map (kbd "C-c \\") #'split-window-horizontally)
                            (define-key term-raw-map (kbd "C-c -")  #'split-window-vertically)
                            (define-key term-raw-map (kbd "C-c l")  #'windmove-right)
                            (define-key term-raw-map (kbd "C-c h")  #'windmove-left)
                            (define-key term-raw-map (kbd "C-c j")  #'windmove-down)
                            (define-key term-raw-map (kbd "C-c k")  #'windmove-up)
                            (define-key term-raw-map (kbd "C-c C-y")  #'term-paste)
                            (setq bidi-paragraph-direction 'left-to-right) ;faster back scrolling
                            (setq show-trailing-whitespace nil)))

(straight-use-package 'eshell-z)

(autoload 'eshell/z "eshell-z")
(add-hook 'eshell-mode-hook (lambda ()
                              (company-mode -1)
                              (setq show-trailing-whitespace nil)))

(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-commands "dstat")
  (add-to-list 'eshell-visual-options '("nmcli" "--ask")))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))
