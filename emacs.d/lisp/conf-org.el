;;; -*- lexical-binding: t; -*-

(defun setup-org-keybindings ()
  (local-set-key "\M-p" 'org-metaup)
  (local-set-key "\M-n" 'org-metadown))
(add-hook 'org-mode-hook 'setup-org-keybindings)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)