;;; -*- lexical-binding: t; -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'flycheck-clojure)
(straight-use-package 'inf-clojure)

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-c C-r") 'projectile-toggle-between-implementation-and-test))))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)


