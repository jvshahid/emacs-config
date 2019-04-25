;;; -*- lexical-binding: t; -*-

(straight-use-package 'flx-ido)

;; The wiki states that this shouldn't be done, but ido ends up not doing some
;; important things, such as remembering the most frequently visited paths and
;; listing them first in the completion list.  To get around this leave
;; ido-mode enabled and disable the ability to disable it using an advice.

(ido-mode t)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; do nothing
(defun shahid/do-not-disable-ido (orig-func &rest args))
(advice-add 'ido-mode :override #'shahid/do-not-disable-ido)
