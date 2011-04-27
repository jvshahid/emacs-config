;; Or enable more if you wish
;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key "\C-c\C-w" 'backward-kill-word)
(add-to-list 'load-path "~/.emacs.d/libs/project-root")
(add-to-list 'load-path "~/.emacs.d/libs/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/libs/scala")
(add-to-list 'load-path "~/.emacs.d/libs/magit")
(load-file "~/.emacs.d/libs/cedet/common/cedet.el")
(load-file "~/.emacs.d/libs/ruby/ruby-electric.el")
(load-file "~/.emacs.d/libs/ruby/ruby-mode.el")
(load-file "~/.emacs.d/libs/markdown-mode/markdown-mode.el")
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))
(require 'magit)
(require 'yaml-mode)
(require 'project-root)
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(column-number-more t)
 '(display-time-mode t)
 '(ido-mode (quote both) nil (ido))
 '(menu-bar-mode nil)
 '(ns-command-modifier (quote control))
 '(scala-mode-feature:electric-newline-before-closing-bracket t)
 '(scala-mode-feature:electric-on-per-default t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(tool-bar-mode nil))
(setq project-roots
      '(("Generic workspace" :root-contains-files (".workspace"))))
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
(define-key global-map (kbd "C-x C-a") 'hs-toggle-hiding)
(define-key global-map (kbd "C-;") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-x C-r") 'query-replace)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x C-e") 'query-replace-regexp)
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)

;; Add ensime
;; Load the ensime lisp code...
(add-to-list 'load-path "~/.emacs.d/libs/ensime/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(global-linum-mode 1)
(add-hook 'magit-mode-hook '(lambda ()
                              (setq show-trailing-whitespace nil)
                              (setq truncate-lines nil)))
(setq linum-format "%d ")
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey" :weight bold :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(cursor ((t (:background "white")))))

; Toggle full screen
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
(add-hook 'scala-mode-hook 'hs-minor-mode)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(display-time)

;; Add ruby mode and ruby electric
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;; Enable ruby electric when ruby-mode is activated
(add-hook 'ruby-mode-hook
          (lambda()
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))
