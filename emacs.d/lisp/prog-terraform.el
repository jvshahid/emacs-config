;; -*- lexical-binding: t; -*-

(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(with-eval-after-load 'terraform-mode
  (company-terraform-init)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-hook 'terraform-mode-hook #'hs-minor-mode)
  (add-hook 'terraform-mode-hook #'subword-mode)
  (add-hook 'terraform-mode-hook #'yas-minor-mode))
