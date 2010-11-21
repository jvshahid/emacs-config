;; Or enable more if you wish
(add-to-list 'load-path "~/.emacs.d/libs/malabar/lisp")
(add-to-list 'load-path "~/.emacs.d/libs/project-root")
(add-to-list 'load-path "~/.emacs.d/libs/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/libs/scala")
(load-file "~/.emacs.d/libs/cedet/common/cedet.el")
(setq malabar-groovy-lib-dir "~/.emacs.d/libs/malabar/lib/")
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semanticdb-minor-mode
				  global-semantic-idle-summary-mode
				  global-semantic-mru-bookmark-mode))
(require 'malabar-mode)
(require 'yaml-mode)
(require 'project-root)
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . malabar-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ido-mode (quote both) nil (ido))
 '(malabar-case-fixed-abbrevs (quote (("pu" "public") ("pri" "private") ("pro" "protected") ("st" "static") ("vo" "void") ("ab" "abstract") ("bo" "boolean") ("cl" "class") ("impl" "implements") ("ext" "extends") ("pa" "package") ("re" "return") ("#Test" malabar-abbrevs-create-test) ("sysout" "System.out.println(\"\")"))))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 2))
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
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default tab-width 2) ; set tab width to 4 for all buffers
(setq-default show-trailing-whitespace t) ; show the trailing whitespace at the end of line (not including the end of line character)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "bitstream" :family "Bitstream Vera Sans Mono")))))
