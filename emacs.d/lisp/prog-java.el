;;; -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)

(setenv "CLASSPATH" "/home/jvshahid/bin/lsp-java/plugins/org.eclipse.equinox.launcher_1.5.200.v20180922-1751.jar")

(add-hook 'java-mode-hook (lambda ()
                            (eglot-ensure)
                            (company-mode)
                            (auto-complete-mode -1)
                            (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                            (local-set-key (kbd "M-.") 'company-complete)))
