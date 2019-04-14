;;; -*- lexical-binding: t; -*-

(straight-use-package 'lsp-java)
(require 'lsp-java)

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'yas-minor-mode)
(add-hook 'java-mode-hook #'hs-minor-mode)
(add-hook 'java-mode-hook (lambda () (setq-local company-idle-delay 0.5)))
