;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  elisp funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq search-exit-option 'move)

(straight-use-package '(popup :type git
                              :host github
                              :repo "jvshahid/popup-el"
                              :branch "fix-unaligned-left-margin")) ; pr#116
(straight-use-package '(debbugs :type git :host github :repo "emacsmirror/debbugs" :files ("*")))
(straight-use-package 'go-mode)
(straight-use-package 'projectile)
(straight-use-package 'magit)
(straight-use-package 'etags-select)
(straight-use-package 'yaml-mode)
(straight-use-package 'go-guru)
(straight-use-package 'markdown-mode)
(straight-use-package 'protobuf-mode)
(straight-use-package 'go-eldoc)
(straight-use-package 'auto-complete)
(straight-use-package '(go-autocomplete :type git :host github :repo "mdempsky/gocode" :files ("emacs/*")))
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'ace-window)
(straight-use-package 'go-rename)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clojure)
(straight-use-package 'paredit)
(straight-use-package 'helm)
(straight-use-package 'request)
(straight-use-package 'ac-cider)
(straight-use-package 'wgrep)
(straight-use-package 'flx-ido)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-ui)
(straight-use-package 'company-lsp)
(straight-use-package '(concourse-mode :type git :host github :repo "jvshahid/concourse-mode"))
(straight-use-package '(pianobar :type git :host github :repo "agrif/pianobar.el"))
(straight-use-package '(ginkgo-mode :type git :host github :repo "jvshahid/ginkgo-mode" :branch "minor-fixes"))
(straight-use-package '(parinfer :type git
                                 :host github
                                 :repo  "DogLooksGood/parinfer-mode"
                                 :branch "master"))
(straight-use-package 'edit-indirect)   ;markdown edit code regions

(defun disable-line-numbers (orig &rest args)
  "set `display-line-numbers' to `nil' in order to make the
popup-create fast. Otherwise the following list of functions will be slow:

- `end-of-visual-line'
- `beginning-of-visual-line'
- `posn-col-row'

all of which are used by popup-create and slowing it down.
"
  (let ((display-line-numbers nil))
    (apply orig args)))

(advice-add 'ac-menu-create :around #'disable-line-numbers)

(global-set-key (kbd "C-c =") #'helm-show-kill-ring)

(with-eval-after-load 'company-lsp
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(with-eval-after-load 'company
  (push 'java-mode company-global-modes)
  (push 'company-lsp company-backends))

(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil))

(global-unset-key (kbd "C-z"))     ;stop suspending the frame on accidental C-z

(autoload 'lsp-java-enable "lsp-java")
(setq lsp-java-server-install-dir "~/bin/lsp-java")
(setq lsp-java--workspace-folders (list "/home/jvshahid/codez/nokogiri/ext/java"
                                        "/home/jvshahid/codez/jruby/core"))

(add-hook 'java-mode-hook (lambda ()
                            (lsp-java-enable)
                            (lsp-ui-mode)
                            (auto-complete-mode -1)
                            (company-mode)
                            (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                            (local-set-key (kbd "M-.") 'company-complete)))

(with-eval-after-load 'parinfer
  (require 'paredit)
  (setq parinfer-extensions
        '(defaults
           pretty-parens
           smart-tab
           paredit)))

(with-eval-after-load 'yasnippet-snippets
  (yas-reload-all))

(flx-ido-mode 1)
(setq ido-use-faces nil)

(winner-mode)
(global-set-key (kbd "C-c \\") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

(defun define-repeatable-key (keyseq key cmd)
  "define a repeatable key sequence. KEYSEQ is the initial key
sequence invoking the command. KEY is the key sequence to invoke
the command again. CMD is the command to run"
  (global-set-key keyseq
                  (lambda ()
                    (interactive)
                    (funcall cmd)
                    (set-transient-map
                     (let ((map (make-sparse-keymap)))
                       (define-key map key 'repeat)
                       map)))))

(define-repeatable-key (kbd "C-c M-=") (kbd "M-=") (lambda () (enlarge-window 4)))
(define-repeatable-key (kbd "C-c M--") (kbd "M--") (lambda () (shrink-window 4)))
(define-repeatable-key (kbd "C-c M-.") (kbd "M-.") (lambda () (enlarge-window-horizontally 4)))
(define-repeatable-key (kbd "C-c M-,") (kbd "M-,") (lambda () (shrink-window-horizontally 4)))
(global-set-key (kbd "C-c C-r") 'ff-find-related-file)

(define-key global-map (kbd "C-x g") 'magit-status)
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook '(lambda ()
                                (setq show-trailing-whitespace nil)))
  (setq magit-revert-item-confirm t))

(with-eval-after-load 'helm
  (setq helm-findutils-search-full-path t))
;;; override the default fzf find command
(setenv "FZF_DEFAULT_COMMAND" "ag -g \"\"")
(global-set-key (kbd "C-x C-p") #'better-fzf)

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))
  (setq flycheck-go-build-install-deps t))
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (setq flycheck-disabled-checkers '(go-megacheck))))

;; disable the use of CGO inside emacs. otherwise, it complains that gcc is
;; missing while flychecking a buffer
(setenv "CGO_ENABLED" "0")

(setenv "GIT_EDITOR" "emacsclient")

(autoload 'aw-swap-window "ace-window")
(defun swap-next-window (n)
  "swap the buffer of the current window with the next window obtained using 'next-window"
  (interactive "p")
  (let ((fun  (if (< n 0) 'previous-window 'next-window))
        (n    (abs n)))
    (dotimes (_ n)
      (aw-swap-window (funcall fun)))))

(defun swap-previous-window (n)
  "swap the buffer of the current window with the previous window obtained using 'previous-window"
  (interactive "p")
  (swap-next-window (- n)))

(xterm-mouse-mode)
(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c ]") 'swap-next-window)
(global-set-key (kbd "C-c [") 'swap-previous-window)

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'projectile-mode)
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook 'flycheck-clojure-setup)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (push 'clojure-cider-typed flycheck-disabled-checkers)))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-x p") 'parinfer-toggle-mode)))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (local-set-key (kbd "C-c C-r") 'projectile-toggle-between-implementation-and-test))))

(add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-x p") 'parinfer-toggle-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-c C-j") 'xref-find-definitions)))
(defun better-fzf ()
  (interactive)
  (let* ((buffer (make-term "*fzf*" "bash" nil "-c" "~/.fzf/bin/fzf -m --height=20"))
         (proc   (get-buffer-process buffer)))
    (set-process-sentinel proc (lambda (proc str)
                                 (goto-char (point-min))
                                 (let (files)
                                   (while (/= (point) (point-max))
                                     (if-let ((file (thing-at-point 'line)))
                                         (unless (string= "" (string-trim file))
                                           (push (expand-file-name (string-trim file))
                                                 files)))
                                     (forward-line))
                                   (when (string= str "finished\n")
                                     (find-file (car files))
                                     (mapc #'find-file-other-window (cdr files)))
                                   (kill-buffer (process-buffer proc)))))
    (switch-to-buffer buffer)
    (term-char-mode)))

(defun show-all-buffers ()
  (interactive)
  (display-buffer (list-buffers-noselect nil (buffer-list))))

(add-hook 'Buffer-menu-mode-hook '(lambda ()
                                    (setq show-trailing-whitespace nil)))

(defun align-elisp-let (begin end)
  (interactive "r")
  (align-regexp begin
                end
                "[a-z]\\(\\s-+\\)\\((\\|[a-z]\\)"))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook 'turn-on-orgstruct)
  (add-hook 'markdown-mode-hook 'setup-org-keybindings)
  (add-hook 'gfm-mode-hook 'setup-org-keybindings)
  (setq-default markdown-command "~/bin/flavor"))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(defun cider-autocomplete-setup ()
  (auto-complete-mode)
  (ac-cider-setup)
  (local-set-key (kbd "C-c C-j") 'cider-find-var))
(add-hook 'cider-mode-hook 'cider-autocomplete-setup)
(add-hook 'cider-mode-hook (lambda ()
                             (define-key cider-mode-map (kbd "C-c M-.") nil)))

(add-hook 'cider-repl-mode-hook 'cider-autocomplete-setup)
(with-eval-after-load 'auto-complete
  (ac-config-default)
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'cider-repl-mode)
  (setq ac-ignore-case t)
  (setq ac-auto-show-menu t)
  (setq ac-auto-start t)
  (setq ac-delay 0.0)
  (add-hook 'auto-complete-mode-hook (lambda ()
                                       (local-set-key "\M-." 'auto-complete))))

(defun remove-pianobar-mode ()
  (setq mode-line-modes (delete pianobar-modeline-object mode-line-modes)))

(with-eval-after-load 'protobuf-mode
  (add-hook 'protobuf-mode-hook 'subword-mode))
(autoload 'pianobar "pianobar" "pianobar pandora mode" t)
(with-eval-after-load 'pianobar
  (setq pianobar-command "~/codez/pianobar/pianobar")
  (add-hook 'pianobar-mode-hook (lambda ()
                                  (add-hook 'kill-buffer-hook
                                            #'remove-pianobar-mode))))

(with-eval-after-load 'arduino-mode
  (add-hook 'arduino-mode-hook
            'subword-mode))

(defun add-yasnippet-to-ac-sources ()
  (push 'ac-source-yasnippet ac-sources))

(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)

;;; end of modes

;;; miscellaneous functions

(defun omg-this-is-a-dos-file ()
  (interactive)
  (revert-buffer-with-coding-system 'us-ascii-dos))

(defun epoch-to-string (seconds)
  (format-time-string "%d %b %Y %H:%M:%S %Z" (seconds-to-time seconds)))

(defun string-to-epoch (str)
  (unix-time (apply 'encode-time (parse-time-string str))))

(defun unix-time (&optional optional-time)
  (let ((time (if optional-time optional-time (current-time))))
    (+ (ash (car time) 16)
       (cadr time))))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defalias 'shahid/range (symbol-function 'number-sequence))

(defun on-linux? ()
  (eq system-type 'gnu/linux))

(defun find-grep-current-word (ignore-case)
  "interactive way to grep for the word under the cursor using
   ack-grep on linux or ag on macos"
  (interactive "P")
  (let* ((extra-arg (if ignore-case "-i " ""))
         (grep-cmd "ag --color --nogroup ")
         (prompt (if ignore-case "search for (ignore case): " "search for: "))
         (word (read-string prompt (current-word)))
         (directory (read-directory-name "in: " default-directory)))
    (grep-find (concat grep-cmd extra-arg (shell-quote-argument word) " " directory))))

;; assign a key to find-grep-current-word
(global-set-key (kbd "C-c C-g") 'find-grep-current-word)

(defun clear-tags-table ()
  (interactive)
  (setq tags-completion-table nil))

(global-set-key (kbd "M-.") 'xref-find-references)
(setq completion-ignore-case t)

(global-auto-revert-mode 1)
(global-set-key "\C-c\C-w" 'backward-kill-word)
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

;; (desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-menu-map t)
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "xdg-open")
 '(c-basic-offset 2)
 '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "gnu")))
 '(cider-lein-parameters "repl :headless :host localhost")
 '(column-number-mode t)
 '(column-number-more t)
 '(debug-on-error nil)
 '(dired-omit-files "\\.test\\|^\\.?#\\|^\\.$\\|^\\.\\.$\\|.*~$")
 '(display-battery-mode t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(electric-indent-mode nil)
 '(erc-user-full-name "John Shahid")
 '(etags-select-use-short-name-completion t)
 '(fill-column 79)
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(ido-mode 'both nil (ido))
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(ns-command-modifier 'control)
 '(perl-indent-level 2)
 '(safe-local-variable-values '((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tags-case-fold-search t)
 '(tool-bar-mode nil)
 '(user-full-name "John Shahid")
 '(user-mail-address "jvshahid@gmail.com")
 '(windmove-wrap-around t))


(when (display-graphic-p)
  (straight-use-package '(color-theme :type git :host github :repo "emacsorphanage/color-theme" :files ("color-theme.el" "themes")))
  (straight-use-package 'color-theme-solarized)
  (require 'color-theme)
  (require 'color-theme-solarized)
  (define-key key-translation-map (kbd "C-8") (kbd "DEL"))
  (server-start)
  (setq frame-background-mode 'dark)
  (load-theme 'solarized t))

(display-time)

(define-key global-map (kbd "C-x C-r") 'query-replace)

(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :height 113 :width normal :family "Ubuntu Mono" :foundry "unknown"))))
 '(ido-first-match ((t (:foreground "yellow4" :weight bold))))
 '(ido-only-match ((t (:foreground "yellow4")))))

;; Adding automatic untabify and delete trailing whitespaces (very useful)
;; (add-hook 'local-write-file-hooks
;;           '(lambda()
;;              (save-excursion
;;                (untabify (point-min) (point-max))
;;                (delete-trailing-whitespace)
;;                )))

(defun toggle-indent-longer-lines (&optional delta)
  "Trigger selective display to hide lines that have more indentation than the current line. \
If DELTA was provided it will be added to the current line's indentation."
  (interactive "P")
  (let ((indentation (current-indentation)))
    (if selective-display
        (set-selective-display nil)
      (set-selective-display (+ indentation 1
                                (if delta delta 0))))))
(define-key global-map (kbd "C-x t") 'toggle-indent-longer-lines)

(defface todo-face '((t :background "red" :foreground "grey"))
  "The face used to mark TODO"
  :group 'todo-faces)
(defvar todo-face "todo-face")
(defun add-todo-font-locking-to-mode ()
  "highlight TODO and FIXME only if font-lock-defaults is set"
  (if font-lock-defaults
      (font-lock-add-keywords nil '(("\\(TODO\\|FIXME\\):" 1 'todo-face prepend)) t)))
(add-hook 'after-change-major-mode-hook 'add-todo-font-locking-to-mode)

;; Omit emacs files from the Dired
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(setq dired-omit-files
      (concat dired-omit-files ".*~$"))

;; add the ace-window mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Simple modes           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; disable org toc
(setq org-export-with-toc nil)
(defun setup-org-keybindings ()
  (local-set-key "\M-p" 'org-metaup)
  (local-set-key "\M-n" 'org-metadown))
(add-hook 'org-mode-hook 'setup-org-keybindings)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq c++fmt-command "clang-format-3.8")
(setq c++fmt-args (lambda (filename)
                    (list (format "-assume-filename=%s" filename) "-style=file")))
(setq arduinofmt-command c++fmt-command)
(setq arduinofmt-args c++fmt-args)
(setq cfmt-command c++fmt-command)
(setq cfmt-args c++fmt-args)

(require 'rect)

(defun apply-to-rectangle (b e f)
  "For the rectangle defined by [B,E] apply the function F to each line with start/end set to the start and end columns."
  (interactive "r\naFunction name to apply: ")
  (apply-on-rectangle 'apply-rectangle-line b e f))

(defun apply-rectangle-line (startcol endcol f)
  "Apply F to the region defined by [STARTCOL, ENDCOL]"
  (when (= (move-to-column startcol) startcol)
    (funcall f
             (point)
             (progn (move-to-column endcol 'coerce)
                    (point)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          GO lang mode               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'go-mode
  (require 'go-eldoc)
  (require 'go-autocomplete)
  (require 'go-rename)
  (require 'go-guru)
  (setq ginkgo-use-pwd-as-test-dir t)
  (setq ginkgo-use-default-keys t)
  (require 'ginkgo-mode)
  (add-hook 'go-mode-hook 'auto-complete-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'hs-minor-mode)
  (add-hook 'go-mode-hook 'ginkgo-mode)
  (add-hook 'go-mode-hook #'add-yasnippet-to-ac-sources))


(defun setup-gopath ()
  (when (and (eq (current-buffer) (window-buffer)) ; filter temp buffer events
             (equal major-mode 'go-mode)
             ;; ensure the buffer has a backing file
             buffer-file-name)
    (unless (boundp 'gopath)
      (if-let ((root (locate-dominating-file buffer-file-name ".envrc")))
          (setq-local gopath (expand-file-name root))
        (setq-local gopath  nil)))
    (save-match-data
      (setenv "GOPATH" gopath))))

(add-hook 'buffer-list-update-hook #'setup-gopath)

(defun add-to-path (path)
  (let ((path (substitute-env-vars path)))
    (setq exec-path (cons path exec-path))
    (setenv "PATH" (concat path ":$PATH") t)))

(autoload 'ff-basename "find-file" "Return the basename of pathname STRING.")

(defun setup-goroot ()
  "Setup the golang environment, this function will install
   goimports, godef, godoc and gocode"
  (interactive)
  (let ((go-cmd (locate-file "go" exec-path)))
    ;; ask the user for the path of go if we can't find it
    (unless go-cmd
      (let* ((goroot (expand-file-name (read-directory-name "GOROOT")))
             (gobin (concat goroot "/bin")))
        (add-to-path gobin)
        (setenv "GOROOT" goroot)))))

(add-to-path "$HOME/.emacs.d/go/bin")

(defun install-go-deps ()
  (interactive)
  (let ((process-environment (cons (substitute-env-vars "GOPATH=$HOME/.emacs.d/go")
                                   process-environment))
        ;; make sure the buffer-list-update-hook don't run, otherwise it could
        ;; mess up the GOPATH environment variables
        (buffer-list-update-hook nil)
        (urls  '("golang.org/x/tools/cmd/goimports"
                 "github.com/rogpeppe/godef"
                 "github.com/nsf/gocode"
                 "github.com/dougm/goflymake"
                 "golang.org/x/tools/cmd/gorename"
                 "golang.org/x/tools/cmd/godoc"
                 "github.com/golang/lint"
                 "github.com/kisielk/errcheck"
                 "github.com/mdempsky/unconvert"
                 "golang.org/x/tools/cmd/guru")))
    (let ((default-directory (substitute-env-vars "$HOME/.emacs.d/go")))
      (apply 'start-process "go-get" "*go-get*" "go" "get" "-u" urls))))

(setq gofmt-command "goimports")
(setq gofmt-args nil)

(global-set-key (kbd "C-c C-x d") 'godoc)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Ruby mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun ruby-insert-end ()
;;   (interactive)
;;   (insert "end")
;;   (ruby-indent-line t)
;;   (end-of-line))

;; (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; Enable ruby electric when ruby-mode is activated
(add-hook 'ruby-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         C/C++mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-c++-hook ()
  (subword-mode))

(add-hook 'c-mode-hook 'c-c++-hook)
(add-hook 'c++-mode-hook 'c-c++-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         useful functions        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         formatting functions        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-symbol-value (&rest names)
  "return the value of the variable whose name is given by concatenating
   all the given arguments or nil if the variable isn't set."
  (let ((s (intern (apply 'concat names))))
    (if (boundp s)
        (eval s))))

(defun fmt-before-save ()
  "Add this to .emacs to format the current buffer when saving:
 (add-hook 'before-save-hook 'fmt-before-save)."

  (interactive)
  (let* ((mode (cl-first (split-string (symbol-name major-mode) "-")))
         (fmt-command (get-symbol-value mode "fmt-command"))
         (fmt-args (get-symbol-value mode "fmt-args")))
    (if fmt-command
        (fmt fmt-command fmt-args))))

(add-hook 'before-save-hook #'fmt-before-save)

;; the following formatting functions are inspired by the go-mode
;; formatting functions.
(defun fmt (fmt-command fmt-args)
  "The functions takes two args, the format command and it's arguments.
   the command is assumed to take the filename as the last argument and
   do the formatting in place"

  (interactive)

  (let* ((filename (buffer-file-name))
         (ext (file-name-extension filename))
         (tmpfile (make-temp-file "fmt" nil ext))
         (patchbuf (get-buffer-create "*fmt patch*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (fmt-arg (if (functionp fmt-args)
                      (funcall fmt-args filename)
                    fmt-args)))


    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    ;; We're using errbuf for the mixed stdout and stderr output. This
    ;; is not an issue because gofmt -w does not produce any stdout
    ;; output in case of success.
    (if (zerop (apply 'call-process-region (point-min) (point-max) fmt-command nil `((:file ,tmpfile) ,tmpfile) nil fmt-arg))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            ;; if diff exit code is 0 then there are no changes
            (progn
              (message "Buffer is already formatted"))
          (fmt-apply-rcs-patch patchbuf)
          (message "Applied formatting"))
      (message "Could not format. Check errors for details"))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defalias 'modified-kill-whole-line
  (if (fboundp 'kill-whole-line)
      #'kill-whole-line
    #'kill-entire-line))

(defun delete-whole-line (&optional arg)
  "Delete the current line without putting it in the kill-ring."
  ;; Emacs uses both kill-region and kill-new, Xemacs only uses
  ;; kill-region. In both cases we turn them into operations that do
  ;; not modify the kill ring. This solution does depend on the
  ;; implementation of kill-line, but it's the only viable solution
  ;; that does not require to write kill-line from scratch.
  (cl-flet ((kill-region (beg end)
                         (delete-region beg end))
            (kill-new (_) ()))
    (modified-kill-whole-line arg)))

(defun fmt-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-line (- from line-offset))
                (message "offset: %d" line-offset)
                (cl-incf line-offset len)
                (message "offset: %d" line-offset)
                (delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in go--apply-rcs-patch")))))))))

(put 'scroll-left 'disabled nil)

;;; email

(add-to-list 'load-path "~/bin/mu/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e" "start mu4e" t)

(defun paste-term ()
  (interactive
   (term-send-string (get-buffer-process (current-buffer))
                     (current-kill 0))))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-/") (lambda ()
                                                                   (interactive)
                                                                   (term-send-raw-string (kbd "C-_"))))
                            (define-key term-raw-map (kbd "C-c \\") #'split-window-horizontally)
                            (define-key term-raw-map (kbd "C-c -")  #'split-window-vertically)
                            (define-key term-raw-map (kbd "C-c l")  #'windmove-right)
                            (define-key term-raw-map (kbd "C-c h")  #'windmove-left)
                            (define-key term-raw-map (kbd "C-c j")  #'windmove-down)
                            (define-key term-raw-map (kbd "C-c k")  #'windmove-up)
                            (define-key term-raw-map (kbd "C-c C-y")  #'paste-term)
                            (setq bidi-paragraph-direction 'left-to-right) ;faster back scrolling
                            (setq show-trailing-whitespace nil)
                            (push (lambda () (display-line-numbers-mode 0))
                                  delayed-after-hook-functions)))

(with-eval-after-load 'mu4e
  (setq mu4e-get-mail-command (expand-file-name "~/bin/isync/bin/mbsync gmail")
        mu4e-html2text-comma 'mu4e-shr2text ; nd "w3m -dump -T text/html"
        mu4e-update-interval nil          ;do not auto update
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include nil
        mu4e-mu-binary (expand-file-name "~/bin/mu/bin/mu")
        mu4e-maildir (expand-file-name "~/Maildir/gmail")
        mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-sent-folder "/[Gmail]/Sent Mail"
        mu4e-sent-message-behavior 'delete
        mu4e-view-show-addresses t
        mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
        mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                                 ("/[Gmail]/Sent Mail" . ?s)
                                 ("/[Gmail]/All Mail" . ?a)))

  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (push (lambda () (display-line-numbers-mode 0))
                    delayed-after-hook-functions)))


  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)
              (push (lambda () (display-line-numbers-mode 0))
                    delayed-after-hook-functions)))

  (require 'org-mu4e)
  (setq org-mu4e-convert-to-html t)

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t))

(defun insert-zapped-char (_ ch)
  (insert-char ch)
  (forward-char -1))

(advice-add 'zap-to-char :after #'insert-zapped-char)

(defun toggle-mic-mute ()
  (interactive)
  (start-process "toggle-audio-mute" nil "amixer" "set" "Capture" "toggle"))

(defun toggle-audio-mute ()
  (interactive)
  (start-process "toggle-audio-mute" nil "amixer" "-D" "pulse" "set" "Master" "1+" "toggle"))

(defun raise-audio-volume ()
  (interactive)
  (start-process "raise-audio-volume" nil "amixer" "-q" "sset" "Master" "5%+"))

(defun lower-audio-volume ()
  (interactive)
  (start-process "raise-audio-volume" nil "amixer" "-q" "sset" "Master" "5%-"))

(global-set-key (kbd "<XF86AudioMute>") #'toggle-audio-mute)
(global-set-key (kbd "<XF86AudioMicMute>") #'toggle-mic-mute)
(global-set-key (kbd "<XF86AudioLowerVolume>") #'lower-audio-volume)
(global-set-key (kbd "<XF86AudioRaiseVolume>") #'raise-audio-volume)


(when (display-graphic-p)
  (straight-use-package '(exwm :type git :host github :repo "ch11ng/exwm"))
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (require 'exwm-config)
  (exwm-config-default)

  (setq exwm-input-simulation-keys `(([?\C-b] . [left])
                                     ([?\C-f] . [right])
                                     ([?\C-p] . [up])
                                     ([?\C-n] . [down])
                                     ([?\C-a] . [home])
                                     ([?\C-e] . [end])
                                     ([?\M-v] . [prior])
                                     ([?\C-v] . [next])
                                     ([?\C-d] . [delete])
                                     ([?\M-b] . [C-left])
                                     ([?\M-f] . [C-right])
                                     ([?\M-d] . [C-S-right C-x])
                                     (,(kbd "DEL") . [backspace])
                                     (,(kbd "C-8") . [backspace])
                                     ([?\C-/] . [C-z])
                                     ([?\C-m] . [return])
                                     ([?\M-<] . [C-home])
                                     ([?\M->] . [C-end])
                                     ([?\C-o] . [return left])
                                     ([?\C-k] . [S-end C-x delete])
                                     ([?\C-y] . [C-v])
                                     ([?\C-w] . [C-x])
                                     ([?\M-w] . [C-c])
                                     ([?\C-c ?\C-w] . [S-C-left C-x])
                                     ([?\C-  ?\C-n] . [S-down])
                                     ([?\C-  ?\C-p] . [S-up])
                                     ([?\C-  ?\M-f] . [S-C-right])
                                     ([?\C-  ?\M-b] . [S-C-left])
                                     ([?\C-  ?\C-a] . [S-home])
                                     ([?\C-  ?\C-e] . [S-end])
                                     ([?\C- ?\M-<] . [S-C-home])
                                     ([?\C- ?\M->] . [S-C-end])))

  (exwm-input--set-simulation-keys exwm-input-simulation-keys)

  (setq exwm-workspace-number 2
        exwm-workspace-switch-create-limit 0)

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-1 --above LVDS-1 --auto")))
  (exwm-randr-enable)

  (exwm-input-set-key (kbd "<XF86AudioMute>") #'toggle-audio-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'toggle-mic-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'lower-audio-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'raise-audio-volume))
