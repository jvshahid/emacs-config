;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  elisp funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(winner-mode)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)

(require 'cask)

(defun time-to-string (seconds)
  (format-time-string "%m/%d/%Y %H:%M:%S %z" (seconds-to-time seconds)))

(defun unix-time ()
  (let ((time (current-time)))
   (+ (ash (car time) 16)
      (cadr time))))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))


(defun Y (f)
  "Y combinator in elisp. You can call it like this

(defun fact (f x)
  (if (= x 1) 1
    (* x (funcall f (- x 1)))))
(funcall (Y 'fact) 10)

or

(funcall (Y (lambda (f x)
              (if (= x 1) 1
                (* x (funcall f (- x 1))))))
         3)
"
  ;; lexical-let is required here since elisp doesn't allow lexical binding,
  ;; i.e. closure (up to version 24.1)
  (lexical-let ((fun f))
    ((lambda (x) (funcall x x))
     (lambda (y)
       (lexical-let ((funy y))
         (lambda (&rest args)
           (apply fun (funcall funy funy) args)))))))

(defun fact (f x)
  (if (= x 1)
      1
    (* x (funcall f (- x 1)))))

(defun fib (f x)
  (cond
   ((= x 1) 1)
   ((= x 2) 1)
    (t (+ (funcall f (- x 1))
          (funcall f (- x 2))))))

(defalias 'shahid/range (symbol-function 'number-sequence))
(defalias 'find-and-replace-in-marked 'dired-do-find-regexp-and-replace)

(defun shahid/flatten (x)
  "return a sequence that is the concatentation of all the
sequences in `x'. The returned sequence is the same type as the first
element in X."
  (let ((first (car x)))
    (cond ((stringp first) (apply 'concat x))
          ((listp first) (apply 'append x)))))

(defun shahid/permutations (X k)
  "Given a list X and a number k, returns a list of all
k-length permutations of elements in X."
  (if (= k 0)
      '(nil)
    (let ((newX (shahid/permutations X (- k 1))))
      (shahid/flatten
       (mapcar (lambda (x) (mapcar (apply-partially 'cons x) newX)) X)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         GLOBAL SETTINGS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-files)
(defun submodule-update (dir)
  "Run `git submodule update --init --recursive' in the given directory"
  (interactive "D")
  (let ((default-directory dir))
    (message "Updating submodules")
    (call-process "git" nil "*Messages*" nil "submodule" "update" "--init" "--recursive")
    (message "Finished updating submodules")))

;; TODO: make sure that this function works properly
(defun enable-auto-save ()
  (interactive)
  (global-auto-revert-mode 1)
  (setq auto-save-timeout 1)
  (setq auto-save-visited-file-name t)


  ;;  move autosave files somewhere out of the directory path
  ;;  http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html
  (defvar user-temporary-file-directory "~/.emacs.d/auto-save")
  (make-directory user-temporary-file-directory t)
  (setq backup-by-copying t)
  (setq backup-directory-alist
        `(("." . ,user-temporary-file-directory)
          (,tramp-file-name-regexp nil)))
  (setq auto-save-list-file-prefix
        (concat user-temporary-file-directory ".auto-saves-"))
  (setq auto-save-file-name-transforms
        `((".*" ,user-temporary-file-directory t))))

(setq tooltip-mode nil)

(defun on-linux? ()
  (eq system-type 'gnu/linux))

(defun find-grep-current-word (ignore-case)
  "interactive way to grep for the word under the cursor using
   ack-grep on linux or ag on macos"
  (interactive "P")
  (let* ((extra-arg (if ignore-case "-i " ""))
         (grep-cmd (if (on-linux?) "ack-grep --color --no-group " "ag --color --nogroup "))
         (prompt (if ignore-case "search for (ignore case): " "search for: "))
         (word (read-string prompt (current-word)))
         (directory (read-directory-name "in: " default-directory)))
    (grep-find (concat grep-cmd extra-arg (shell-quote-argument word) " " directory))))

;; assign a key to find-grep-current-word
(global-set-key (kbd "C-c C-g") 'find-grep-current-word)

(defun clear-tags-table ()
  (interactive)
  (setq tags-completion-table nil))

(defun find-prog (filename program)
  (let ((dir (file-name-directory filename))
        (filename nil))
    (while (and (not (string= dir "/")) (not filename))
      (let ((filename-temp (file-truename (concat dir "/" program))))
        (if (file-exists-p filename-temp)
            (setq filename filename-temp)
            (setq dir (file-truename (concat dir "/.."))))))
    filename))

(defun refresh-tags ()
  (interactive)
  (let ((refresh-tags-sh (find-prog buffer-file-name "refresh_tags.sh")))
    (if refresh-tags-sh
        (progn
          (message "refreshing tags using %s" refresh-tags-sh)
          (if (= 0 (call-process-shell-command refresh-tags-sh))
              (progn
                (message "finished refreshing the tags")
                (clear-tags-table))
            (error "process exit with non zero exit code")))
      (error "Couldn't find refresh_tags.sh in any directory above %s" buffer-file-name))))

(global-set-key (kbd "C-c C-t") 'refresh-tags) ; move to left windnow

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names nil nil nil))))

(global-set-key (kbd "M-.") 'etags-select-find-tag)
(setq completion-ignore-case t)

(global-auto-revert-mode 1)
;; (global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(global-set-key "\C-c\C-w" 'backward-kill-word)
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"

;; (desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "xdg-open")
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(column-number-more t)
 '(debug-on-error nil)
 '(dired-omit-files "\\.test\\|^\\.?#\\|^\\.$\\|^\\.\\.$\\|.*~$")
 '(display-battery-mode t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(eclim-eclipse-dirs (quote ("~/bin/eclipse")))
 '(eclim-executable "~/bin/eclipse/eclim")
 '(electric-indent-mode nil)
 '(erc-user-full-name "John Shahid")
 '(etags-select-use-short-name-completion t)
 '(fill-column 79)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-go-build-install-deps t)
 '(gnus-select-method (quote (nnmaildir "GMAIL" (directory "~/Maildir/"))))
 '(godoc-command "godoc")
 '(godoc-use-completing-read t)
 '(helm-findutils-search-full-path t)
 '(ido-mode (quote both) nil (ido))
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(ns-command-modifier (quote control))
 '(perl-indent-level 2)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tags-case-fold-search t)
 '(tool-bar-mode nil)
 '(user-mail-address "jvshahid@gmail.com                      ")
 '(windmove-wrap-around t))

(require 'flycheck)
(setq-default flycheck-disabled-checkers '(go-errcheck))

(set-quit-char ?q)
(when (display-graphic-p)
	(require 'color-theme)
	(require 'color-theme-solarized)
	(server-start)
	(setq frame-background-mode 'dark)
	(add-to-list 'custom-theme-load-path "~/.emacs.d/libs/color-theme-solarized")
	(load-theme 'solarized t))

(display-time)

(define-key global-map (kbd "C-;") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-x C-r") 'query-replace)

(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)

(global-linum-mode 1)
(setq linum-format "%d ") ;adds an extra space after line number

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

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [f11] 'toggle-fullscreen)
                                        ; TODO: for some reason highlighting doesn't work in JavaDoc comments
                                        ; that start with /**
(defface todo-face '((t :background "red" :foreground "grey"))
  "The face used to mark TODO"
  :group 'todo-faces)
(defvar todo-face "todo-face")
(defun add-todo-font-locking-to-mode ()
  (font-lock-add-keywords nil (list (cons "\\(TODO\\|FIXME\\):" (list 1 'todo-face 'prepend))) nil))
(add-hook 'after-change-major-mode-hook 'add-todo-font-locking-to-mode)

;; Omit emacs files from the Dired
(require 'dired-x)
(dired-omit-mode 1)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(setq dired-omit-files
      (concat dired-omit-files ".*~$"))

;; add the ace-window mode
(require 'ace-window)

(global-set-key (kbd "C-'") 'ace-jump-mode)
(global-set-key "\C-xo" 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Simple modes           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.glj$" . clojure-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'haskell-mode)
(require 'yasnippet)
(require 'yaml-mode)
(require 'protobuf-mode)
(require 'pianobar)
(require 'arduino-mode)
(require 'clojure-mode)
(require 'cider)
(require 'flycheck-clojure)
(require 'markdown-mode)
(require 'paredit)
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'arduino-mode-hook
          (lambda ()
            (subword-mode)))

(setq pianobar-command "~/codez/pianobar/pianobar")

(add-hook 'edit-server-start-hook
          (lambda ()
            (if (string-match "wiki*" (buffer-name))
                (progn
                  (outline-minor-mode)
                  (setq outline-regexp "h1\\|h2\\.\\|h3\\. ")))))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)
(add-hook 'markdown-mode-hook 'turn-on-orgstruct)
(defun setup-org-keybindings ()
  (local-set-key "\M-p" 'org-metaup)
  (local-set-key "\M-n" 'org-metadown))
(add-hook 'org-mode-hook 'setup-org-keybindings)
(add-hook 'gfm-mode-hook 'setup-org-keybindings)
(add-hook 'markdown-mode-hook 'setup-org-keybindings)
(setq markdown-command "~/bin/flavor")
;; (add-hook 'markdown-mode-hook 'turn-on-orgstruct++)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.xaml$" . xml-mode))
(require 'livedown)

(setq c++fmt-command "clang-format-3.8")
(setq c++fmt-args (lambda (filename)
                    (list (format "-assume-filename=%s" filename) "-style=file")))
(setq arduinofmt-command c++fmt-command)
(setq arduinofmt-args c++fmt-args)
(setq cfmt-command c++fmt-command)
(setq cfmt-args c++fmt-args)
(setq javafmt-command c++fmt-command)
(setq javafmt-args c++fmt-args)

(defun apply-to-rectangle (b e f)
  "For the rectangle defined by [B,E] apply the function F to each line with start/end set to the start and end columns."
  (interactive "r\naFunction name to apply: ")
  (apply-on-rectangle 'apply-rectangle-line f b e))

(defun apply-rectangle-line (f startcol endcol)
  "Apply F to the region defined by [STARTCOL, ENDCOL]"
  (when (= (move-to-column startcol) startcol)
    (funcall f
           (point)
           (progn (move-to-column endcol 'coerce)
                  (point)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          Concourse mode               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/concourse-mode")
(require 'concourse-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          GO lang mode               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'go-eldoc)
(require 'go-autocomplete)
(require 'go-rename)
(require 'go-guru)

(defun disable-auto-completion ()
  (setq-local ac-auto-start nil)
  (local-set-key "\M-." 'ac-start))

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'auto-complete-mode)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook (lambda ()
                          (yas-minor-mode)
                          (yas-reload-all)))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'disable-auto-completion)
(add-hook 'go-mode-hook 'hs-minor-mode)

(defun add-to-path (path)
  (setq exec-path (cons path exec-path))
  (setenv "PATH" (concat path ":$PATH") t))

(add-to-path "/usr/local/bin")

(defun find-go ()
  (let* ((goroot (expand-file-name (read-directory-name "GOROOT")))
         (gobin (concat goroot "/bin")))
    (add-to-path gobin)
    (setenv "GOROOT" goroot)))

(defun setup-go-path ()
  (let* ((gopath (expand-file-name (read-directory-name "GOPATH")))
         (gopathbin (concat gopath "/bin")))
    (add-to-path gopathbin)
    (setenv "GOPATH" gopath)
    (setenv "GOBIN" gopathbin)))

(defun setup-go-env ()
  "Setup the golang environment, this function will install
   goimports, godef, godoc and gocode"
  (interactive)
  (let ((go-cmd (locate-file "go" exec-path)))

    ;; ask the user for the path of go if we can't find it
    (if (not go-cmd) (find-go))

    (setup-go-path)

    ;; install goimports, godef, godoc and gocode
    (dolist (url '("golang.org/x/tools/cmd/goimports"
                   "github.com/rogpeppe/godef"
                   "golang.org/x/tools/cmd/godoc"
                   "github.com/nsf/gocode"
                   "github.com/dougm/goflymake"
                   "golang.org/x/tools/cmd/gorename"
                   "golang.org/x/tools/cmd/godoc"
                   "github.com/golang/lint"
                   "github.com/mdempsky/unconvert"
                   "golang.org/x/tools/cmd/guru"))
      (message "Running 'go get -u %s'" url)
      (if (/= 0(call-process "go" nil "*go-get*" nil "get" url))
          (error "Cannot run go get")))))

(setq gofmt-command "goimports")
(setq gofmt-args nil)

(global-set-key (kbd "C-c C-x d") 'godoc)
(global-set-key (kbd "C-x C-p") 'helm-find)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Ruby mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ruby-mode)
(require 'rvm)
(rvm-use-default)

;; (defun ruby-insert-end ()
;;   (interactive)
;;   (insert "end")
;;   (ruby-indent-line t)
;;   (end-of-line))

(defun ruby-brace-to-do-end ()
  "convert a {-} block into a do-end block"
  (when (looking-at "{")
    (let ((orig (point))
          (end  (progn
                  (ruby-forward-sexp)
                  (set-mark (point))
                  (point))))
      (when (eq (char-before) ?\})
        (delete-char -1)
        (insert "\nend")
        (goto-char orig)
        (delete-char 1)
        (insert "do")
        (when (looking-at ".*\\s |.*|")
          (message "found args")
          (goto-char (match-end 0)))
        (insert "\n")
        (indent-region orig (+ (mark) 2))
        (delete-trailing-whitespace orig (+ (mark) 2))
        t))))

(defun ruby-do-end-to-brace ()
  "opposite of ruby-brace-to-do-end"
  (when (and (or (bolp)
                 (not (memq (char-syntax (char-before)) '(?w ?_))))
             (looking-at "\\<do\\(\\s \\|$\\)"))
    (let ((orig (point)) (end (progn (ruby-forward-sexp) (point))))
      (backward-char 3)
      (when (looking-at ruby-block-end-re)
        (delete-char 3)
        (insert "}")
        (goto-char orig)
        (delete-char 2)
        (insert "{")
        (if (looking-at "\\s +|")
            (delete-char (- (match-end 0) (match-beginning 0) 1)))
        t))))

(defun ruby-toggle-block ()
  "toggle block mode do-end -> {} or {} -> do-end"
  (interactive)
  (or (ruby-brace-to-do-end)
      (ruby-do-end-to-brace)))


;; (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; Enable ruby electric when ruby-mode is activated
(add-hook 'ruby-mode-hook
          (lambda()
            (subword-mode)))

(setq ruby-deep-indent-paren nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Java mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook 'flycheck-init)
(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'yas-reload-all)
(add-hook 'java-mode-hook 'subword-mode)
(require 'eclim)

(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim)
(ac-emacs-eclim-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         C/C++mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-c++-hook ()
  (subword-mode))

(add-hook 'c-mode-hook 'c-c++-hook)
(add-hook 'c++-mode-hook 'c-c++-hook)

(defun insert-newline-before-curlies (action pair pos-before)
  (progn
    (cond ((eq pair ?})
           (newline-and-indent)
           (save-excursion
             (newline-and-indent))
           (indent-for-tab-command)))))

(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook '(lambda ()
                              (font-lock-mode 0)
                              (setq show-trailing-whitespace nil)))

(setq magit-revert-item-confirm t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Shell mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook '(lambda ()
                               (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook '(lambda ()
                             (setq show-trailing-whitespace nil)
                             (linum-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         useful functions        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun omg-this-is-a-dos-file ()
  (interactive)
  (revert-buffer-with-coding-system 'us-ascii-dos))
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
  (let* ((mode (first (split-string (symbol-name major-mode) "-")))
         (fmt-command (get-symbol-value mode "fmt-command"))
         (fmt-args (get-symbol-value mode "fmt-args")))
    (if (and fmt-command)
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
      (message "Could not format. Check errors for details")
      (find-file tmpfile))

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
         (kill-new (s) ()))
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
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-line (- from line-offset))
                (incf line-offset len)
                (delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in go--apply-rcs-patch")))))))))

(put 'scroll-left 'disabled nil)
