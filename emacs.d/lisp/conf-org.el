;;; -*- lexical-binding: t; -*-

(defun setup-org-keybindings ()
  (local-set-key "\M-p" 'org-metaup)
  (local-set-key "\M-n" 'org-metadown))
(add-hook 'org-mode-hook 'setup-org-keybindings)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-c C-a") #'org-agenda)

(with-eval-after-load 'org
  (setq org-refile-targets '((nil . (:level . 1)) (nil . (:level . 2)))
        org-agenda-text-search-extra-files '("~/Dropbox/orgs/todo.org_archive")
        org-agenda-custom-commands '(("c" . "My Custom Agendas")
                                     ("cu" "Unscheduled TODO"
                                      ((todo ""
                                             ((org-agenda-overriding-header "\nUnscheduled TODO")
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
                                      nil
                                      nil))
        org-agenda-skip-scheduled-if-done t
        org-directory "~/Dropbox/orgs"
        org-enforce-todo-dependencies t
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
        org-log-into-drawer t
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/orgs/todo.org" "Inbox")
           "* TODO %? %^g\n  %a")))
  (add-to-list 'org-agenda-files "~/Dropbox/orgs"))

(define-key global-map "\C-cc" #'org-capture)
