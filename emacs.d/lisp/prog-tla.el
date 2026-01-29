(straight-use-package '(tla-mode :type git
                                 :host github
                                 :repo  "ratish-punnoose/tla-mode"))
(require 'tla-mode)
(defun tla-disable-pretty-symbols ()
  (prettify-symbols-mode -1))
(add-hook 'tla-mode-hook #'tla-disable-pretty-symbols)
