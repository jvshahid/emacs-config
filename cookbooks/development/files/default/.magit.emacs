;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         MaGit mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/libs/dash/dash.el")
(add-to-list 'load-path "~/.emacs.d/libs/with-editor")
(add-to-list 'load-path "~/.emacs.d/libs/magit-mode/lisp")
;;(add-to-list 'load-path "~/.emacs.d/libs/git-modes")
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook '(lambda ()
                              (font-lock-mode 0)
                              (setq show-trailing-whitespace nil)))
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/.emacs.d/libs/magit-mode/Documentation/"))


(setq magit-revert-item-confirm t)
