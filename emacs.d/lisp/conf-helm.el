;;; -*- lexical-binding: t; -*-

(ido-mode -1)                           ; turn off ido-mode

(straight-use-package 'ivy)
(straight-use-package 'counsel)

(ivy-mode)

(setq projectile-completion-system 'ivy)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-x C-p") 'projectile-find-file)
(global-set-key (kbd "M-s o") 'swiper-isearch)
