;;; -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'hs-minor-mode)
(defun shahid/java-mode-hook ()
  (setq-local c-basic-offset 2))
(add-hook 'java-mode-hook #'shahid/java-mode-hook)
