;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/bin/mu/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e" "start mu4e" t)

(with-eval-after-load 'mu4e
  (require 'org-mu4e))

;; prevent mu4e update messages
(setq mu4e-hide-index-messages t)

(shahid/bind-global-key "s-m" #'mu4e)

;; keep email details separately in an encrypted file.  Ignore errors if the
;; file is encrypted
(ignore-errors
  (load "email-setup.el"))
