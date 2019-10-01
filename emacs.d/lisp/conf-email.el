;;; -*- lexical-binding: t; -*-

(defun shahid/read-auth-passwd (host user)
  (let ((found (nth 0 (auth-source-search :max 1
                                          :host host
                                          :user user
                                          :require '(:secret)))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(add-to-list 'load-path "~/bin/mu/share/emacs/site-lisp/mu4e")
(autoload 'mu4e "mu4e" "start mu4e" t)

(straight-use-package 'helm-mu)

(add-to-list 'Info-additional-directory-list "~/bin/mu/share/info")

(with-eval-after-load 'mu4e
  (require 'org-mu4e))

;; prevent mu4e update messages
(setq mu4e-hide-index-messages t
      mu4e-display-update-status-in-modeline t)

(shahid/bind-global-key "s-m" #'mu4e)

;; keep email details separately in an encrypted file.  Ignore errors if the
;; file is encrypted
(ignore-errors
  (load "email-setup.el"))
