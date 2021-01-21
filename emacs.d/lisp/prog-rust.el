;;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(add-hook 'rust-mode-hook (lambda ()
                            (eglot-ensure)
                            (yas-minor-mode)
                            (company-mode)))
