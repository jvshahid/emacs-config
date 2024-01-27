;;; -*- lexical-binding: t; -*-

;; configuration for term and eshell

(require 'cl)

;; use inhibit-same-window for eshell buffers
;; inhibit-same-window
(add-to-list 'display-buffer-alist
             '("\\*eshell.*" display-buffer-reuse-window (inhibit-same-window . t)))

(straight-use-package '(aweshell :type git :host github :repo "manateelazycat/aweshell"))
(with-eval-after-load 'eshell
  (require 'aweshell)
  ;; Undo the change that aweshell makes
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-l") nil))
            100))

(autoload 'aweshell-switch-buffer "aweshell")

(setq aweshell-auto-suggestion-p nil)

(defun shahid/disable-complete-in-region (&rest args)
  (completion-in-region-mode -1))

(advice-add 'eshell-send-input :before #'shahid/disable-complete-in-region)
;;(straight-use-package 'eat)
(straight-use-package 'eshell-z)

(setq eshell-prefer-lisp-functions t)
(autoload 'eshell/z "eshell-z")
(add-hook 'eshell-mode-hook (lambda ()
                              (setq show-trailing-whitespace nil)))

(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-commands "dstat")
  (add-to-list 'eshell-visual-options '("nmcli" "--ask")))

(setq eshell-buffer-maximum-lines 10000)
(with-eval-after-load 'esh-mode
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-hist-mode-map (kbd "M-r") 'counsel-esh-history)))

(defun eshell-confirm-kill (func &rest args)
  (when (y-or-n-p (format "Kill eshell process "))
    (apply func args)))

(advice-add 'eshell-kill-process :around #'eshell-confirm-kill)

(straight-use-package 'eat)
(add-hook 'eshell-load-hook #'eat-eshell-mode)


