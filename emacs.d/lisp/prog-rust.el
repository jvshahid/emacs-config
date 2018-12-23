;;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(add-hook 'rust-mode-hook (lambda ()
                            (eglot-ensure)
                            (yas-minor-mode)
                            (auto-complete-mode -1)
                            (company-mode)
                            (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                            (local-set-key (kbd "M-.") 'company-complete)))
