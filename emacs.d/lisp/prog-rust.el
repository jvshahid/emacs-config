;;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(add-hook 'rust-mode-hook (lambda ()
                            (lsp)
                            (yas-minor-mode)
                            (company-mode)))

