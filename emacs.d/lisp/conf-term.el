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

(setq term-buffer-maximum-size 10000)

(defun shahid/disable-complete-in-region (&rest args)
  (completion-in-region-mode -1))

(advice-add 'eshell-send-input :before #'shahid/disable-complete-in-region)
(add-hook 'term-mode-hook (lambda ()
                            ;; fix C-c non-prefix error when eshell runs visual commands
                            (term-set-escape-char ?\C-c)
                            (define-key term-raw-map (kbd "C-/") (lambda ()
                                                                   (interactive)
                                                                   (term-send-raw-string (kbd "C-_"))))
                            (define-key term-raw-map (kbd "C-c \\") #'split-window-horizontally)
                            (define-key term-raw-map (kbd "C-c -")  #'split-window-vertically)
                            (define-key term-raw-map (kbd "C-c l")  #'windmove-right)
                            (define-key term-raw-map (kbd "C-c h")  #'windmove-left)
                            (define-key term-raw-map (kbd "C-c j")  #'windmove-down)
                            (define-key term-raw-map (kbd "C-c k")  #'windmove-up)
                            (define-key term-raw-map (kbd "C-c C-y")  #'term-paste)
                            (setq bidi-paragraph-direction 'left-to-right) ;faster back scrolling
                            (setq show-trailing-whitespace nil)))

(straight-use-package 'eshell-z)

(setq eshell-prefer-lisp-functions t)
(autoload 'eshell/z "eshell-z")
(add-hook 'eshell-mode-hook (lambda ()
                              (company-mode -1)
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
