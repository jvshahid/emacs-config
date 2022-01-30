;;; -*- lexical-binding: t; -*-

(ido-mode -1)                           ; turn off ido-mode

(straight-use-package 'ivy)
(straight-use-package 'ivy-hydra)
(straight-use-package 'counsel)
(straight-use-package 'ivy-rich)

(setq ivy-virtual-abbreviate 'abbreviate
      ivy-use-virtual-buffers t
      ivy-rich-parse-remote-buffer nil
      counsel-find-file-at-point t
      counsel-find-file-ignore-regexp "\\(?:\\[#.]\\)\\|\\(?:[#~]\\)")

(ivy-mode)
(counsel-mode)
(ivy-rich-mode)

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq show-trailing-whitespace nil)))

(setq ivy-display-functions-alist '(t)
      ivy-rich-parse-remote-file-path nil)

(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
(global-set-key (kbd "C-c i") 'ivy-resume)

(setq projectile-completion-system 'ivy)
(global-set-key (kbd "C-x C-p") 'projectile-find-file)
(global-set-key (kbd "M-s o") 'swiper-isearch-thing-at-point)
