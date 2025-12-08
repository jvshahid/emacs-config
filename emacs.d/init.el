;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  elisp funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-menu-map t)
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff"
    "#eeeeec"])
 '(auth-source-save-behavior nil)
 '(auto-revert-use-notify nil)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "xdg-open")
 '(c-basic-offset 4)
 '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "gnu")))
 '(column-number-mode t)
 '(column-number-more t)
 '(custom-safe-themes
   '("e823e0188ab19377659d40195933fbdaa6aa09c3d79002f00cb497336728dc0a" default))
 '(debug-on-error nil)
 '(dired-omit-files "\\.test\\|^\\.?#\\|^\\.$\\|^\\.\\.$\\|.*~$")
 '(display-battery-mode t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-indent-mode nil)
 '(erc-user-full-name "John Shahid")
 '(eshell-cmpl-ignore-case t)
 '(etags-select-use-short-name-completion t)
 '(fill-column 79)
 '(global-font-lock-mode t)
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(ns-command-modifier 'control)
 '(perl-indent-level 2)
 '(recentf-max-saved-items 200)
 '(safe-local-variable-values
   '((bug-reference-url-format . https://www.pivotaltracker.com/stories/show/1)
     (imenu-create-index-function . concourse-imenu-index-function)
     (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))
 '(sc-auto-fill-region-p nil)
 '(sc-citation-leader "")
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tags-case-fold-search t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(wgrep-auto-save-buffer t)
 '(windmove-wrap-around t)
 '(yank-excluded-properties t))

;; don't enable show-trailing-space for special/read-only buffers
(defun shahid/disable-show-trailing-space ()
  (setq show-trailing-whitespace nil))
(add-hook 'special-mode-hook #'shahid/disable-show-trailing-space)

(setq recentf-auto-cleanup 'never)
(recentf-mode)
(run-with-idle-timer 60 t #'recentf-save-list)

(push "~/.emacs.d/lisp" load-path)
;; (load "conf-exwm")
(load "navigation")
(load "conf-email")
(load "conf-magit")
(load "format-on-save")
(load "conf-fuzzy")
(load "conf-org")
(load "conf-term")
(load "conf-mc")

(define-key key-translation-map (kbd "C-8") (kbd "DEL"))

;; programming modes
(load "prog-autocomplete")
(load "prog-ruby")
(load "prog-go")
(load "prog-rust")
(load "prog-elisp")
(load "prog-clojure")
(load "prog-java")
(load "prog-c")
(load "prog-terraform")
(load "prog-lsp")
(load "prog-claude")

(global-set-key (kbd "C-c r l") #'tldr)
(global-set-key (kbd "C-c r e") #'aweshell-switch-buffer)
(global-set-key (kbd "C-c r n") #'aweshell-new)
(global-set-key (kbd "C-c r t") #'eat)
(global-set-key (kbd "C-c r m") #'mu4e)

(with-eval-after-load 'yasnippet-snippets
  (yas-reload-all))

(global-set-key (kbd "C-c C-r") 'ff-find-related-file)

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled))))

(setenv "GIT_EDITOR" "emacsclient")
;; Don't use a pager inside eshell or ansi-term, I can use Emacs binding to
;; navigate in the output history
(setenv "PAGER" "cat")

(defun show-all-buffers ()
  (interactive)
  (display-buffer (list-buffers-noselect nil (buffer-list))))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(autoload 'orgtbl-setup "org-table")

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'orgtbl-setup)
  (add-hook 'markdown-mode-hook 'setup-org-keybindings)
  (add-hook 'gfm-mode-hook 'setup-org-keybindings)
  (setq-default markdown-command "~/bin/flavor"))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(defun add-yasnippet-to-ac-sources ()
  (push 'ac-source-yasnippet ac-sources))

;;; end of modes

;;; miscellaneous functions

(defun omg-this-is-a-dos-file ()
  (interactive)
  (revert-buffer-with-coding-system 'us-ascii-dos))

(defun epoch-to-string (start end)
  (interactive "r")
  (let ((seconds (string-to-number (buffer-substring start end))))
    (delete-region start end)
    (insert (format-time-string "%d %b %Y %H:%M:%S %Z"
                                (seconds-to-time seconds)))))

(defun string-to-epoch (str)
  (unix-time (apply 'encode-time (parse-time-string str))))

(defun unix-time (&optional optional-time)
  (let ((time (if optional-time optional-time (current-time))))
    (+ (ash (car time) 16)
       (cadr time))))

(defalias 'shahid/range (symbol-function 'number-sequence))

(defun find-grep-current-word (ignore-case)
  "interactive way to grep for the word under the cursor using
ag"
  (interactive "P")
  (let* ((extra-arg (if ignore-case "-i " ""))
         (grep-cmd "rg --color=never --no-heading ")
         (prompt (if ignore-case "search for (ignore case): " "search for: "))
         (word (read-string prompt (current-word)))
         (directory (read-directory-name "in: " default-directory)))
    (let ((default-directory directory))
      (grep-find (concat grep-cmd extra-arg (shell-quote-argument word))))))

;; assign a key to find-grep-current-word
(global-set-key (kbd "C-c C-g") 'find-grep-current-word)

(global-set-key (kbd "C-x 4 j") #'find-file-at-point)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(setq completion-ignore-case t)

(global-set-key (kbd "C-c C-w") 'backward-kill-word)
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

;; set spaces between lines for easier readability
(setq-default line-spacing 2)

;; display time in mode line

(define-key global-map (kbd "C-x C-r") 'query-replace)

(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)

(defun toggle-indent-longer-lines (&optional delta)
  "Trigger selective display to hide lines that have more indentation than the current line.
If DELTA was provided it will be added to the current line's
indentation."
  (interactive "P")
  (let ((indentation (current-indentation)))
    (if selective-display
        (set-selective-display nil)
      (set-selective-display (+ indentation 1
                                (if delta delta 0))))))

(define-key global-map (kbd "C-x t") 'toggle-indent-longer-lines)
(define-key global-map (kbd "M-u") #'upcase-dwim)
(define-key global-map (kbd "M-l") #'downcase-dwim)
(define-key global-map (kbd "M-c") #'capitalize-dwim)

;; Omit emacs files from the Dired
(require 'dired-x)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      (concat dired-omit-files ".*~$"))

;; override dired bindings
(global-set-key (kbd "C-x C-j") #'projectile-switch-project)

;;; disable org toc
(setq org-export-with-toc nil)

(put 'narrow-to-region 'disabled nil)

;;; enable scroll-left command without confirmation
(put 'scroll-left 'disabled nil)

(global-set-key (kbd "M-z") #'zap-up-to-char)

(setq ring-bell-function 'ignore)

;; Add the version of Emacs when a symbol was added
;; Stefan Monnier: http://lists.gnu.org/archive/html/emacs-devel/2018-09/msg00959.html
(defun help-fns--first-release (function)
  "Try and find the first release that defined this function."
  ;; Code below relies on the etc/NEWS* files.
  ;; FIXME: Maybe we should also use the */ChangeLog* files when available.
  ;; FIXME: Maybe we should also look for announcements of the addition
  ;; of the *packages* in which the function is defined.
  (when (symbolp function)
    (let* ((name (symbol-name function))
           (re (concat "\\_<" (regexp-quote name) "\\_>"))
           (news (directory-files data-directory t "\\`NEWS.[1-9]"))
           (first nil))
      (with-temp-buffer
        (dolist (f news)
          (erase-buffer)
          (insert-file-contents f)
          (goto-char (point-min))
          (search-forward "\n*")
          (while (re-search-forward re nil t)
            (save-excursion
              (if (re-search-backward "^\\*.*in Emacs \\([0-9.]+\\)"
                                      nil t)
                  ;; (error "Ref found in non-versioned section in %S"
                  ;;        (file-name-nondirectory f))
                (let ((version (match-string 1)))
                  (when (or (null first) (version< version first))
                    (setq first version))))))))
      (when first
        (insert (format "\nIntroduced at or before Emacs version %s.\n"
                        first))))))

(add-hook 'help-fns-describe-function-functions #'help-fns--first-release)

(put 'list-timers 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(global-set-key (kbd "C-c m") #'imenu)
(defun concourse-imenu-index-function ()
  (let (title
        elems
        index)
    (beginning-of-buffer)
    (save-match-data
      (while (search-forward-regexp "^\\([^: \n]*\\):$\\|^- name: \\(.*\\)$" nil t)
        (if (match-string 2)
            (push (cons (match-string-no-properties 2)
                        (line-beginning-position))
                  elems)
          (when title
            (push (cons title (reverse elems)) index))
          (setq elems nil)
          (setq title (match-string-no-properties 1))))
      (push (cons title (seq-reverse elems)) index)
      index)))

(defun setup-concourse-index-function ()
  (interactive)
  (add-dir-local-variable 'yaml-mode 'imenu-create-index-function #'concourse-imenu-index-function))

(push '(imenu-create-index-function . concourse-imenu-index-function) safe-local-variable-values)

(setq-default imenu-auto-rescan t)

(setq gc-cons-threshold 800000)
