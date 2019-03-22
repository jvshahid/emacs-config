;;; -*- lexical-binding: t; -*-

(straight-use-package 'haskell-mode)

(with-eval-after-load 'haskell-mode
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'haskell-collapse-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
