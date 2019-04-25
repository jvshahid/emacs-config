;;; -*- lexical-binding: t; -*-

(straight-use-package 'flx-ido)

;; The wiki states that this shouldn't be done, but ido ends up not doing some
;; important initialization.  This causes Ido not to remember the most
;; frequently visited paths which is really a cool feature.
(ido-mode t)

;; let helm know that I want to use ido instead
(with-eval-after-load 'helm-mode
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . ido))
  (add-to-list 'helm-completing-read-handlers-alist '(find-grep-current-word . ido)))

(with-eval-after-load 'ido
  (flx-ido-mode 1)
  (setq ido-use-faces nil))
