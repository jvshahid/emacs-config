;;; -*- lexical-binding: t; -*-

(straight-use-package 'rust-mode)

(add-hook 'rust-mode-hook (lambda ()
                            (setq-local eglot-workspace-configuration '((rust . ((goto_def_racer_fallback . t)))))
                            (eglot-ensure)
                            (yas-minor-mode)
                            (company-mode)
                            (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                            (local-set-key (kbd "C-.") 'company-complete)))

