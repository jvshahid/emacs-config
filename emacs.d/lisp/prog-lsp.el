;;; -*- lexical-binding: t; -*-

(straight-use-package 'lsp-ui)
(straight-use-package 'company-lsp)
(setq lsp-inhibit-message t
      lsp-ui-doc-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))
