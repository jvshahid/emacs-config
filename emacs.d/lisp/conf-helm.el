;;; -*- lexical-binding: t; -*-

(ido-mode -1)                           ; turn off ido-mode

(straight-use-package 'ivy)
(straight-use-package 'ivy-hydra)

(setq ivy-virtual-abbreviate 'abbreviate
      ivy-use-virtual-buffers t)

(ivy-mode)

(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

(setq projectile-completion-system 'ivy)
(global-set-key (kbd "C-x C-p") 'projectile-find-file)
