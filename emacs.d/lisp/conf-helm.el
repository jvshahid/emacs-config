;;; -*- lexical-binding: t; -*-

(add-to-list 'display-buffer-alist
             '("\\*helm.*" display-buffer-in-side-window
               (side . bottom)
               (window-height . 20)))

(ido-mode -1)                           ; turn off ido-mode

(straight-use-package 'helm)
(straight-use-package 'helm-projectile)

;; helm fuzzy matching
(setq helm-completion-style 'emacs)
(push 'flex completion-styles)
(setq helm-locate-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-mode-fuzzy-match t)

(setq helm-buffer-skip-remote-checking t)
(setq helm-ff-skip-boring-files t)
(setq helm-grep-file-path-style 'relative)
(setq helm-list-directory-function 'helm-list-dir-lisp)
(setq helm-substitute-in-filename-stay-on-remote t)
(setq helm-prevent-escaping-from-minibuffer nil)

(require 'helm-projectile)
(require 'helm-config)

(global-set-key (kbd "C-c =") #'helm-show-kill-ring)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(setq helm-exit-idle-delay 0)

;; stop using a new frame for helm completion
(setq helm-show-completion-display-function nil)

;;; override the default fzf find command
(global-set-key (kbd "C-x C-p") 'helm-projectile-find-file)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(helm-mode 1)

(global-set-key (kbd "M-s o") 'helm-occur)

