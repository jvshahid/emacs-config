;;; -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'hs-minor-mode)
(add-hook 'java-mode-hook (lambda () (setq-local company-idle-delay 0.5)))
