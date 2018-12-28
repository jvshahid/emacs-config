;;; -*- lexical-binding: t; -*-

(straight-use-package 'clojure-mode)
(straight-use-package 'flycheck-clojure)
(straight-use-package 'cider)

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'projectile-mode)
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook 'flycheck-clojure-setup)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-x p") 'parinfer-toggle-mode)))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-c C-r") 'projectile-toggle-between-implementation-and-test))))

(defun cider-autocomplete-setup ()
  (define-key cider-mode-map (kbd "C-.") 'company-complete)
  (define-key cider-mode-map (kbd "C-c C-j") 'cider-find-dwim))

(add-hook 'cider-mode-hook 'cider-autocomplete-setup)

(add-hook 'cider-repl-mode-hook 'cider-autocomplete-setup)

