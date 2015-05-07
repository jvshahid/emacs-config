;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         GLOBAL SETTINGS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(add-to-list 'load-path "~/.emacs.d/libs/project-root")
(add-to-list 'load-path "~/.emacs.d/libs/edit-emacs-server")
(load-file "~/.emacs.d/libs/markdown-mode/markdown-mode.el")
(load-file "~/.emacs.d/libs/textile-mode/textile-mode.el")
(load-file "~/.emacs.d/libs/crontab/crontab.el")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

;; load haskell mode
(load-file "~/.emacs.d/libs/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'js-mode-hook 'subword-mode)

;; (load-file "~/.emacs.d/libs/git-commit/git-commit.el")
;; (load-file "~/.emacs.d/libs/fold-dwim/fold-dwim.el")
;; (require 'fold-dwim)
;; (setq fold-dwim-outline-style-default 'nested)
;; (global-set-key (kbd "C-c t") 'fold-dwim-toggle)
;; (global-set-key (kbd "C-c e") 'fold-dwim-hide-all)
;; (global-set-key (kbd "C-c s") 'fold-dwim-show-all)

(global-set-key (kbd "C-c C-x l") 'windmove-left) ; move to left windnow
(global-set-key (kbd "C-c C-x r") 'windmove-right) ; move to left windnow
(global-set-key (kbd "C-c C-x u") 'windmove-up) ; move to left windnow
;; (global-set-key (kbd "C-c C-x d") 'windmove-down) ; move to left windnow

;; bind some useful flymake commands
(global-set-key (kbd "C-x C-g d") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-x C-g n") 'flymake-goto-next-error)

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

;; (require 'project-root)
;; (setq project-roots
;;       '(("Generic workspace" :root-contains-files (".workspace"))))
(require 'flymake)
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
(require 'hungry-delete)
(require 'crontab-mode)

(global-auto-revert-mode 1)
(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(setq magit-revert-item-confirm t)
(global-set-key "\C-c\C-w" 'backward-kill-word)
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no"
(if window-system
    (progn
      (server-start)
      (add-to-list 'load-path "~/.emacs.d/libs/color-theme")
      (add-to-list 'load-path "~/.emacs.d/libs/color-theme-solarized")
      (require 'color-theme)
      (require 'color-theme-solarized)
      (color-theme-initialize)
      (color-theme-solarized-light))
  nil)

(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "gnome-open")
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(column-number-more t)
 '(debug-on-error nil)
 '(display-battery-mode t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(electric-indent-mode nil)
 '(erc-user-full-name "John Shahid")
 '(etags-select-use-short-name-completion t)
 '(ido-mode (quote both) nil (ido))
 '(js-indent-level 2)
 '(c-basic-offset 4)
 '(menu-bar-mode nil)
 '(ns-command-modifier (quote control))
 '(perl-indent-level 2)
 '(scala-mode-feature:electric-newline-before-closing-bracket t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tags-case-fold-search t)
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(display-time)

(defun my-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root project-files tbl)
    (unless project-details (project-root-fetch))
    (setq my-project-root (cdr project-details))
    ;; get project files
    (setq project-files
          (split-string
           (shell-command-to-string
            (concat "find "
                    my-project-root
                    " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E -v \"\.(pyc)$\""
                    )) "\n"))
    ;; populate hash table (display repr => path)
    (setq tbl (make-hash-table :test 'equal))
    (let (ido-list)
      (mapc (lambda (path)
              ;; format path for display in ido list
              (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
              ;; strip project root
              (setq key (replace-regexp-in-string my-project-root "" key))
              ;; remove trailing | or /
              (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
              (puthash key path tbl)
              (push key ido-list)
              )
            project-files
            )
      (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))

(define-key global-map [f6] 'my-ido-project-files)
;;(define-key global-map (kbd "C-x C-a") 'hs-toggle-hiding)
(define-key global-map (kbd "C-;") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-x C-r") 'query-replace)
(define-key global-map (kbd "C-x C-e") 'query-replace-regexp)
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)

(global-linum-mode 1)
(setq linum-format "%d ")

(if (on-linux?)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:slant normal
                           :weight normal
                           :height 120
                           :width normal
                           :family "Ubuntu Mono"
                           :foundry "unknown"))))))

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
;; (dired-omit-mode 1)
;; (setq dired-omit-files
;;       (concat dired-omit-files ".*~$"))

(defun fix-flymake-face ()
  (set-face-attribute 'flymake-errline nil :underline "red" :background nil)
  (set-face-attribute 'flymake-warnline nil :underline "blue" :background nil))

;; add the ace-window mode
(add-to-list 'load-path "~/.emacs.d/libs/ace-jump-mode")
(add-to-list 'load-path "~/.emacs.d/libs/ace-window")
(require 'ace-window)
(global-set-key "\M-ss" 'ace-jump-mode)
(global-set-key "\C-xo" 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Simple modes           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/yasnippet")
(add-to-list 'load-path "~/.emacs.d/libs/haml")
(add-to-list 'load-path "~/.emacs.d/libs/protocol-buffers")
(add-to-list 'load-path "~/.emacs.d/libs/cmake")
(add-to-list 'load-path "~/.emacs.d/libs/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/libs/confluence-el")
(add-to-list 'load-path "~/.emacs.d/libs/pianobar")
(add-to-list 'load-path "~/.emacs.d/libs/coffee-mode")
(add-to-list 'load-path "~/.emacs.d/libs/lua-mode")
(add-to-list 'load-path "~/.emacs.d/libs/arduino-mode")

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'yasnippet)
(require 'yaml-mode)
(require 'haml-mode)
(require 'cmake-mode)
(require 'protobuf-mode)
(require 'confluence)
(require 'pianobar)
(require 'arduino-mode)
(add-hook 'arduino-mode-hook
          (lambda ()
            (subword-mode)))

(setq pianobar-command "~/codez/pianobar/pianobar")

(require 'coffee-mode)
(add-hook 'coffee-mode-hook
          (lambda()
            (subword-mode)))

(add-hook 'edit-server-start-hook
          (lambda ()
            (if (string-match "wiki*" (buffer-name))
                (progn
                  (confluence-edit-mode)
                  (local-set-key "\C-c\C-o" 'confluence-get-page-at-point)
                  (outline-minor-mode)
                  (setq outline-regexp "h1\\|h2\\.\\|h3\\. ")))))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.scaml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.xaml$" . xml-mode))

(setq c++fmt-command "astyle")
(setq c++fmt-args (list "--suffix=none" "--style=stroustrup" "-j" "-s4" "-U" "-p" "-c" "-k3" "-Y"))
(setq arduinofmt-command "astyle")
(setq arduinofmt-args c++fmt-args)
(setq cfmt-command "astyle")
(setq cfmt-args c++fmt-args)
(setq javafmt-command "astyle")
(setq javafmt-args (list "--suffix=none" "--style=java" "-j" "-s2" "-U" "-p" "-c" "-Y"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          GO lang mode               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/popup")
(add-to-list 'load-path "~/.emacs.d/libs/auto-complete")
(add-to-list 'load-path "~/.emacs.d/libs/go-mode")
(add-to-list 'load-path "~/.emacs.d/libs/go-eldoc")
(add-to-list 'load-path "~/.emacs.d/libs/gocode/emacs")
(add-to-list 'load-path "~/.emacs.d/libs/go-flymake")
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'go-eldoc)
(require 'go-autocomplete)
(require 'go-mode-autoloads)
(require 'go-flymake)
(require 'go-flycheck)

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'auto-complete-mode)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'fix-flymake-face)

(defun find-go ()
  (let* ((goroot (expand-file-name (read-directory-name "GOROOT")))
         (gobin (concat goroot "/bin")))
    (setenv "PATH" (concat gobin ":$PATH") t)
    (setq exec-path (cons gobin exec-path))
    (setenv "GOROOT" goroot)))

(defun setup-go-path ()
  (let* ((gopath (expand-file-name (read-directory-name "GOPATH")))
         (gopathbin (concat gopath "/bin")))
    (setenv "PATH" (concat gopathbin ":$PATH") t)
    (setenv "GOPATH" gopath)
    (setq exec-path (cons gopathbin exec-path))))

;; go get code.google.com/p/go.tools/cmd/goimports
;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/nsf/gocode
(defun setup-go-env ()
  "Setup the golang environment, this function will install
   goimports, godef, godoc and gocode"
  (interactive)
  (let ((go-cmd (locate-file "go" exec-path)))

    ;; ask the user for the path of go if we can't find it
    (if (not go-cmd) (find-go))

    (setup-go-path)

    ;; install goimports, godef, godoc and gocode
    (dolist (url '("code.google.com/p/go.tools/cmd/goimports"
                   "code.google.com/p/rog-go/exp/cmd/godef"
                   "golang.org/x/tools/cmd/godoc"
                   "github.com/nsf/gocode"
                   "github.com/dougm/goflymake"))
      (message "Running 'go get -u %s" url)
      (if (/= 0(call-process "go" nil "*Messages*" nil "get" url))
          (error "Cannot run go get")))))

(setq gofmt-command "goimports")
(setq gofmt-args (list "-w"))

(global-set-key (kbd "C-c C-x d") 'godoc)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Rust mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/rust-mode")
(require 'rust-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Ruby mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/rvm")
(require 'rvm)
(rvm-use-default)

(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(defun ruby-brace-to-do-end ()
  (when (looking-at "{")
    (let ((orig (point)) (end (progn (ruby-forward-sexp) (point))))
      (when (eq (char-before) ?\})
        (delete-char -1)
        (if (eq (char-syntax (char-before)) ?w)
            (insert " "))
        (insert "end")
        (if (eq (char-syntax (char-after)) ?w)
            (insert " "))
        (goto-char orig)
        (delete-char 1)
        (if (eq (char-syntax (char-before)) ?w)
            (insert " "))
        (insert "do")
        (when (looking-at "\\sw\\||")
          (insert " ")
          (backward-char))
        t))))

(defun ruby-do-end-to-brace ()
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
  (interactive)
  (or (ruby-brace-to-do-end)
      (ruby-do-end-to-brace)))



(load-file "~/.emacs.d/libs/ruby/ruby-electric.el")
;; (load-file "~/.emacs.d/libs/ruby/ruby-mode.el")

;; Add hide show support for ruby
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|class\\|module\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

;; (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; Enable ruby electric when ruby-mode is activated
(add-hook 'ruby-mode-hook
          (lambda()
            (hs-minor-mode)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            (subword-mode)))

(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list (file-truename "~/.emacs.d/check_ruby_script.sh") (list local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\([^:]*\\):\\([0-9]+\\):?\\(?:[0-9]+:\\)?\\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          (function
           (lambda ()
             ;; (fset 'my-ruby-comment
             ;;       [?\C-o tab escape ?1 ?0 ?# return tab ?# ?  ?  return tab escape ?1 ?0 ?# ?\C-p])
             ;; (fset 'my-ruby-comment2
             ;;       [?\C-o tab ?# return tab ?# ?  ?  return tab ?# ?\C-p ?\C-e])
             ;; (local-set-key "\C-cc" 'my-ruby-comment)
             ;; (local-set-key "\C-cv" 'my-ruby-comment2)
             ;; (setq case-fold-search t) ;; this should be in ruby-mode, imho.
             ;; (local-set-key "\t" 'indent-for-tab-command) ;; this should be in ruby-mode, imho.
             ;; (setq ruby-deep-indent-paren nil)
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (progn
                   (flymake-mode)
                   (fix-flymake-face)))
             )))
(add-hook 'ruby-mode-hook 'turn-on-hungry-delete-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Java mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook
          (lambda ()
            (subword-mode)
            (go-flymake-init)
            (go-mode-flymake-hook)
            (fix-flymake-face)
            (setq c-basic-offset 2)))
(push '(".+\\.java$" flymake-go-init) flymake-allowed-file-name-masks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         C/C++mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/libs/google-c-style")
;; (require 'google-c-style)

(defun c-c++-hook ()
  (google-set-c-style)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         MaGit mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/magit-mode")
(add-to-list 'load-path "~/.emacs.d/libs/git-modes")
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook '(lambda ()
                              (setq show-trailing-whitespace nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Shell mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook '(lambda ()
                               (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook '(lambda ()
                              (setq show-trailing-whitespace nil)))
(add-hook 'term-mode-hook '(lambda ()
                             (setq global-hl-line-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         C#/F# mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/fsharp/")
(add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

(setq inferior-fsharp-program "~/Downloads/FSharp-2.0.0.0/bin/fsi.exe --readline-")
(setq fsharp-compiler "~/Downloads/FSharp-2.0.0.0/bin/fsc.exe")

(add-hook 'fsharp-mode-hook 'subword-mode)

;; C#

(add-to-list 'load-path "~/.emacs.d/libs/csharp/")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (yas/minor-mode-on))
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
(add-hook 'fsharp-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Matlab mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/matlab-emacs")
(load-library "matlab-load")


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
    (if (and fmt-command fmt-args)
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
         (errbuf (get-buffer-create "*fmt Errors*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    ;; We're using errbuf for the mixed stdout and stderr output. This
    ;; is not an issue because gofmt -w does not produce any stdout
    ;; output in case of success.
    (if (zerop (apply 'call-process fmt-command nil errbuf nil `(,@fmt-args ,tmpfile)))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            ;; if diff exit code is 0 then there are no changes
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already formatted"))
          (fmt-apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied formatting"))
      (message "Could not format. Check errors for details")
      (display-buffer errbuf))

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
