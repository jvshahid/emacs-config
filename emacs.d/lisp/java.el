(straight-use-package 'lsp-java)

(autoload 'lsp-java-enable "lsp-java")
(setq lsp-java-server-install-dir "~/bin/lsp-java")
(setq lsp-java--workspace-folders (list "/home/jvshahid/codez/nokogiri/ext/java"
                                        "/home/jvshahid/codez/jruby/core"))

(add-hook 'java-mode-hook (lambda ()
                            (lsp-java-enable)
                            (auto-complete-mode -1)
                            (company-mode)
                            (lsp-ui-mode)
                            (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                            (local-set-key (kbd "M-.") 'company-complete)))
