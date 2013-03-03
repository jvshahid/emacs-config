;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         GLOBAL SETTINGS        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "PATH" (concat "/home/jvshahid/bin/go/bin/:" (getenv "PATH")))
(setq tooltip-mode nil)
(add-to-list 'load-path "~/.emacs.d/libs/project-root")
(add-to-list 'load-path "~/.emacs.d/libs/edit-emacs-server")
(load-file "~/.emacs.d/libs/markdown-mode/markdown-mode.el")
(load-file "~/.emacs.d/libs/textile-mode/textile-mode.el")
(load-file "~/.emacs.d/libs/crontab/crontab.el")

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
(global-set-key (kbd "C-c C-x d") 'windmove-down) ; move to left windnow

(defun find-grep-current-word ()
  (interactive)
  (let ((word (current-word)))
    (grep-find (concat "ack-grep --color --no-group " (current-word)))))
(global-set-key (kbd "C-c C-a") 'find-grep-current-word) ; move to left windnow

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; (require 'project-root)
;; (setq project-roots
;;       '(("Generic workspace" :root-contains-files (".workspace"))))
(require 'flymake)
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
(require 'hungry-delete)
(require 'crontab-mode)

(global-auto-revert-mode)
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
(yas/global-mode 1)

(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "gnome-open")
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(column-number-more t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(erc-user-full-name "John Shahid")
 '(flymake-log-level -1)
 '(forml-mode-flymake t)
 '(forml-mode-forml-path "/home/jvshahid/codez/forml/dist/build/forml/forml")
 '(ido-mode (quote both) nil (ido))
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(ns-command-modifier (quote control))
 '(perl-indent-level 2)
 '(scala-mode-feature:electric-newline-before-closing-bracket t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :height 120 :width normal :family "Ubuntu Mono")))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Simple modes           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/haml")
(add-to-list 'load-path "~/.emacs.d/libs/protocol-buffers")
(add-to-list 'load-path "~/.emacs.d/libs/cmake")
(add-to-list 'load-path "~/.emacs.d/libs/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/libs/confluence-el")
(add-to-list 'load-path "~/.emacs.d/libs/pianobar")
(add-to-list 'load-path "~/.emacs.d/libs/coffee-mode")
(add-to-list 'load-path "~/.emacs.d/libs/forml-mode")
(add-to-list 'load-path "~/.emacs.d/libs/lua-mode")

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'yaml-mode)
(require 'haml-mode)
(require 'cmake-mode)
(require 'protobuf-mode)
(require 'confluence)
(require 'pianobar)
(setq pianobar-command "~/codez/pianobar/pianobar")

(require 'forml-mode)
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          GO mode               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "GOROOT" (file-truename "~/bin/go"))
(add-to-list 'load-path "~/.emacs.d/libs/go")

(require 'go-mode-load)
(add-hook 'before-save-hook #'gofmt-before-save)

(defun go-mode-flymake-hook ()
  (when (and go-flymake-script-path (eq major-mode 'go-mode))
              (flymake-start-syntax-check)))
(add-hook 'after-save-hook 'go-mode-flymake-hook)

(defun find-go-flymake (filename)
  (let ((dir (file-name-directory filename))
        (filename nil))
    (while (and (not (string= dir "/")) (not filename))
      (let ((filename-temp (file-truename (concat dir "/flymake.sh"))))
        (if (file-exists-p filename-temp)
            (setq filename filename-temp)
            (setq dir (file-truename (concat dir "/.."))))))
    filename))

(defun go-flymake-init ()
  (let ((path (find-go-flymake buffer-file-name)))
    (setq-local go-flymake-script-path path)))

(add-hook 'go-mode-hook
          (lambda ()
            (go-flymake-init)
            (go-mode-flymake-hook)
            (subword-mode)))

(defun flymake-go-init ()
  (list go-flymake-script-path (list (file-relative-name buffer-file-name))))
(push '(".+\\.go$" flymake-go-init) flymake-allowed-file-name-masks)

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
(push '("^\\([^:]*\\):\\([0-9]+\\):\\(?:[0-9]+:\\)? \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

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
                 (flymake-mode))
             )))
(add-hook 'ruby-mode-hook 'turn-on-hungry-delete-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Java mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         C/C++mode              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/libs/autopair/autopair.el")
(add-to-list 'load-path "~/.emacs.d/libs/google-c-style")
(require 'google-c-style)

(defun c-c++-hook ()
  (autopair-mode)
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

(add-hook 'c++-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'insert-newline-before-curlies))))
(add-hook 'c-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'insert-newline-before-curlies))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Scala mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/scala")
;;(add-to-list 'load-path "~/.emacs.d/libs/ensime/elisp/")
(add-to-list 'load-path "~/.emacs.d/libs/ensime_head/elisp/")

(require 'ensime)
(require 'scala-mode-auto)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'subword-mode)
(add-hook 'scala-mode-hook 'scala-electric-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'scala-mode-hook 'turn-on-hungry-delete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;          Java mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Clojure mode           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         MaGit mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/magit")
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook '(lambda ()
                              (setq show-trailing-whitespace nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         Shell mode             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'eshell-mode-hook '(lambda ()
                               (setq show-trailing-whitespace nil)))

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
