;;; -*- lexical-binding: t; -*-

(straight-use-package 'company)

(setq company-global-modes '(not eshell-mode))
(global-company-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq-default company-idle-delay 0.5)
  (setq company-minimum-prefix-length 0)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-w") 'company-show-location)
  (delete 'company-eclim company-backends)
  (define-key company-mode-map (kbd "C-M-i") 'company-complete))

;; this backend adds so much garbage and is not very usefull.  I usually use
;; M-/ to expand dabbrev
(setq company-backends (delete 'company-dabbrev company-backends))

(defun shahid/add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook #'shahid/add-pcomplete-to-capf)
