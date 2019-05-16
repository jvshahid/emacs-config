;; -*- lexical-binding: t; -*-

(straight-use-package 'terraform-mode)
(straight-use-package 'company-terraform)
(with-eval-after-load 'terraform
  (company-terraform-init))
