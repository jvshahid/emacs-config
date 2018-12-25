;;; -*- lexical-binding: t; -*-

(straight-use-package 'company)

(global-company-mode)

(with-eval-after-load 'company
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 0)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-w") 'company-show-location)
  (delete 'company-eclim company-backends)
  (add-hook 'company-mode-hook (lambda ()
                                 (local-set-key "\M-." 'company-complete))))
