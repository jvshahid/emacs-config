;;; -*- lexical-binding: t; -*-

(setq mouse-autoselect-window t)
(setq vc-follow-symlinks t)
(setq gc-cons-threshold (* 64 1024 1024))

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Fixes the "Feature provided by other file: project" error
;; This has to be the first thing to avoid loading the built-in version of Emacs
(straight-use-package 'project)
(require 'project)
;; end of fix

(straight-use-package 'projectile)
(straight-use-package 'etags-select)
(straight-use-package 'string-inflection)
(straight-use-package 'markdown-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'wgrep)
(straight-use-package 'edit-indirect)   ;markdown edit code regions
(straight-use-package 'direnv)
(straight-use-package 'hydra)
(straight-use-package 'lv)
(straight-use-package 'minions)
(straight-use-package 'tldr)

(minions-mode 1)

(setq-default enable-recursive-minibuffers t)

;; human readable sizes in dired
(setq dired-listing-switches "-alh"
      dired-dwim-target t)

;; setup projectile
(setq projectile-mode-line-prefix "") ; Disable projectile lighter.  Must be set before loading
(setq projectile-dynamic-mode-line nil) ; Disable projectile lighter
(setq projectile-enable-caching t)    ; Turn on caching for faster invocations.
(projectile-mode)
(setq vc-handled-backends nil)

(with-eval-after-load 'direnv
  (setq direnv-always-show-summary t
        direnv-non-file-modes '(eshell-mode)))
(direnv-mode)

(setq completion-ignore-case t)
(setq isearch-yank-on-move t)
(display-time)
(savehist-mode)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(global-unset-key (kbd "C-z"))     ;stop suspending the frame on accidental C-z
(global-unset-key (kbd "C-x C-z")) ;stop suspending the frame on accidental C-x C-z
(global-set-key (kbd "C-x C-v") #'revert-buffer)
(setq-default inhibit-startup-screen t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
(load-theme 'just-grey t)

;; Keep custom-set-variables in a separate file so M-x customize doesn't modify init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; don't enable show-trailing-space for special/read-only buffers
(defun shahid/disable-show-trailing-space ()
  (setq show-trailing-whitespace nil))
(add-hook 'special-mode-hook #'shahid/disable-show-trailing-space)
(add-hook 'term-mode-hook #'shahid/disable-show-trailing-space)

(setq recentf-auto-cleanup 'never)
(recentf-mode)
(run-with-idle-timer 60 t #'recentf-save-list)

(push "~/.emacs.d/lisp" load-path)
(load "navigation")
(load "conf-email")
(load "conf-magit")
(load "conf-fuzzy")
(load "conf-org")
(load "conf-term")
(load "conf-mc")

(define-key key-translation-map (kbd "C-8") (kbd "DEL"))

;; common programming mode setup
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; programming modes
(load "prog-lsp")
(load "prog-autocomplete")
(load "prog-ruby")
(load "prog-go")
(load "prog-rust")
(load "prog-elisp")
(load "prog-clojure")
(load "prog-java")
(load "prog-c")
(load "prog-terraform")
(load "prog-claude")
(load "prog-tla")

(global-set-key (kbd "C-c r l") #'tldr)
(global-set-key (kbd "C-c r e") #'aweshell-switch-buffer)
(global-set-key (kbd "C-c r n") #'aweshell-new)
(global-set-key (kbd "C-c r t") #'eat)
(global-set-key (kbd "C-c r m") #'mu4e)

(with-eval-after-load 'yasnippet-snippets
  (yas-reload-all))

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(setenv "GIT_EDITOR" "emacsclient")
;; Don't use a pager inside eshell or ansi-term, I can use Emacs binding to
;; navigate in the output history
(setenv "PAGER" "cat")

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(autoload 'orgtbl-setup "org-table")

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'orgtbl-setup)
  (add-hook 'markdown-mode-hook 'setup-org-keybindings)
  (add-hook 'gfm-mode-hook 'setup-org-keybindings)
  (setq-default markdown-command "~/bin/flavor"))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(global-set-key (kbd "C-x 4 j") #'find-file-at-point)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "C-c C-w") #'backward-kill-word)
(global-set-key (kbd "C-c C-r") #'ff-find-related-file)
(global-set-key (kbd "C-x C-r") #'query-replace)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "C-c m") #'imenu)
(global-set-key (kbd "C-x C-j") #'projectile-switch-project)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default line-spacing 2)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default imenu-auto-rescan t)
(setq ring-bell-function 'ignore)
(setq org-export-with-toc nil)

;; Omit emacs files from the Dired
(require 'dired-x)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      (concat dired-omit-files ".*~$"))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(load "conf-utils")

(setq gc-cons-threshold 800000)
