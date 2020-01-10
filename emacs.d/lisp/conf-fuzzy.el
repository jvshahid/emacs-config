;;; -*- lexical-binding: t; -*-

(ido-mode -1)                           ; turn off ido-mode

(straight-use-package 'ivy)
(straight-use-package 'ivy-hydra)
(straight-use-package 'counsel)
(straight-use-package 'ivy-rich)

(setq ivy-virtual-abbreviate 'abbreviate
      ivy-use-virtual-buffers t
      counsel-find-file-at-point t
      counsel-find-file-ignore-regexp "\\(?:\\[#.]\\)\\|\\(?:[#~]\\)")

(ivy-mode)
(counsel-mode)
(ivy-rich-mode)

(setq ivy-display-functions-alist '(t))

(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

(setq projectile-completion-system 'ivy)
(global-set-key (kbd "C-x C-p") 'projectile-find-file)
(global-set-key (kbd "M-s o") 'swiper-isearch-thing-at-point)
