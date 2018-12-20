(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company-lsp)

(with-eval-after-load 'company-lsp
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(with-eval-after-load 'company
  (push 'java-mode company-global-modes)
  (push 'company-lsp company-backends))

(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil))
