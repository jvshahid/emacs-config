;;; -*- lexical-binding: t; -*-

(straight-use-package 'evil)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-collection)
(setq evil-want-keybinding nil)
(setq evil-want-C-i-jump nil)

(evil-mode)
(global-evil-surround-mode)

(add-to-list 'evil-emacs-state-modes 'exwm-mode)
(setq evil-emacs-state-modes (delete 'magit-commit-mode evil-emacs-state-modes))
(setq evil-emacs-state-modes (delete 'git-commit-mode evil-emacs-state-modes))

(global-undo-tree-mode -1)
(evil-collection-init '(dired eshell term))

(straight-use-package 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(with-eval-after-load 'evil-org
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))
