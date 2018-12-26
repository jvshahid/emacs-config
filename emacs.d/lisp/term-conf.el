;;; -*- lexical-binding: t; -*-

;; configuration for term and eshell

(add-hook 'term-mode-hook (lambda ()
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

