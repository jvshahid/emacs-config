;;; -*- lexical-binding: t; -*-

(defun remove-pianobar-mode ()
  (setq mode-line-modes (delete pianobar-modeline-object mode-line-modes)))

(with-eval-after-load 'pianobar
  (setq pianobar-command "~/codez/pianobar/pianobar")
  (add-hook 'pianobar-mode-hook (lambda ()
                                  (add-hook 'kill-buffer-hook
                                            #'remove-pianobar-mode))))


(straight-use-package '(pianobar :type git :host github :repo "agrif/pianobar.el"))
(straight-use-package 'emms)

(with-eval-after-load 'emms
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))
