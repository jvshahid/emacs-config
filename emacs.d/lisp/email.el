(add-to-list 'load-path "~/bin/mu/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e" "start mu4e" t)

;; keep email details separately in an encrypted file.  Ignore errors if the
;; file is encrypted
(ignore-errors
  (load "email-setup.el"))
