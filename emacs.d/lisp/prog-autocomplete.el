;;; -*- lexical-binding: t; -*-

(straight-use-package 'company)

(global-company-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 0)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-w") 'company-show-location)
  (delete 'company-eclim company-backends)
  (add-hook 'company-mode-hook (lambda ()
                                 (local-set-key (kbd "C-M-i") 'company-complete))))
