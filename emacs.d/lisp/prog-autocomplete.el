;;; -*- lexical-binding: t; -*-

(straight-use-package 'company)

(global-company-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 0)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-w") 'company-show-location)
  (delete 'company-eclim company-backends)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete))

;; this backend adds so much garbage and is not very usefull.  I usually use
;; M-/ to expand dabbrev
(setq company-backends (delete 'company-dabbrev company-backends))
