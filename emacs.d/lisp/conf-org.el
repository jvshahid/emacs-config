;;; -*- lexical-binding: t; -*-

(defun setup-org-keybindings ()
  (local-set-key "\M-p" 'org-metaup)
  (local-set-key "\M-n" 'org-metadown))
(add-hook 'org-mode-hook 'setup-org-keybindings)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-c C-a") #'org-agenda)

(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/orgs")
  (add-to-list 'org-agenda-files "~/Dropbox/orgs")
  (setq org-enforce-todo-dependencies t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-log-into-drawer t)
  (setq org-capture-templates
   '(("t" "Todo" entry (file+headline "~/Dropbox/orgs/todo.org" "Inbox")
          "* TODO %? %^g\n  %a"))))

(setq org-refile-targets '((nil . (:level . 1)) (nil . (:level . 2))))
(define-key global-map "\C-cc" #'org-capture)
