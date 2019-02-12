;;; -*- lexical-binding: t; -*-

(straight-use-package 'helm)
(straight-use-package 'helm-projectile)

;; helm fuzzy matching
(setq helm-locate-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-mode-fuzzy-match t)
(setq helm-eshell-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(require 'helm-projectile)
(require 'helm-config)

(global-set-key (kbd "C-c =") #'helm-show-kill-ring)
(global-set-key (kbd "M-x") #'helm-M-x)
(setq helm-exit-idle-delay 0)

;; stop using a new frame for helm completion
(setq helm-show-completion-display-function nil)

;;; override the default fzf find command
(global-set-key (kbd "C-x C-p") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
