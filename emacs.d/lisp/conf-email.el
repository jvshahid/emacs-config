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

(defun shahid/mu4e-headers-search-list ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (list (mu4e-message-field (mu4e-message-at-point) :mailing-list)))
    (mu4e-headers-search-narrow (concat "list:" list))))

(defun shahid/mu4e-headers-search-from ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (from (mu4e-message-field msg :from)))
    (mu4e-headers-search-narrow (concat "from:" (cdar from)))))

(with-eval-after-load 'mu4e-headers
	(define-key mu4e-headers-mode-map
		(kbd "s")
		(make-sparse-keymap))

	(define-key mu4e-headers-mode-map
		(kbd "s s")
		'mu4e-headers-search)

	(define-key mu4e-headers-mode-map
		(kbd "s f")
		#'shahid/mu4e-headers-search-from)

	(define-key mu4e-headers-mode-map
		(kbd "s l")
		#'shahid/mu4e-headers-search-list))

;; keep email details separately in an encrypted file.  Ignore errors if the
;; file is encrypted
(ignore-errors
  (load "email-setup.el"))
