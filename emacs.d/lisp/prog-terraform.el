;; -*- lexical-binding: t; -*-

(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(with-eval-after-load 'terraform-mode
  (company-terraform-init)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))
