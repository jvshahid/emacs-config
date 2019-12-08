;;; -*- lexical-binding: t; -*-

(straight-use-package 'cider)
(straight-use-package 'clojure-mode)
(straight-use-package 'flycheck-clojure)

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook 'hs-minor-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-c C-r") 'projectile-toggle-between-implementation-and-test))))

(with-eval-after-load 'cider
  (flycheck-clojure-setup))

(with-eval-after-load 'cider-mode
  (define-key cider-mode-map (kbd "C-M-i") nil))


