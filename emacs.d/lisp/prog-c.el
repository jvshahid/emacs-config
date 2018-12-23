;;; -*- lexical-binding: t; -*-

(straight-use-package 'arduino-mode)

(defun c-c++-hook ()
  (subword-mode))

(add-hook 'c-mode-hook (lambda ()
                         (local-set-key (kbd "C-c C-j") 'xref-find-definitions)
                         (local-set-key (kbd "C-c C-c") 'compile)
                         (local-set-key (kbd "C-c C-m") 'man)))
(add-hook 'c-mode-hook 'c-c++-hook)
(add-hook 'c++-mode-hook 'c-c++-hook)

(setq c++fmt-command "clang-format-8")
(setq c++fmt-args (lambda (filename)
                    (list (format "-assume-filename=%s" filename) "-style=file")))
(setq arduinofmt-command c++fmt-command)
(setq arduinofmt-args c++fmt-args)
(setq cfmt-command c++fmt-command)
(setq cfmt-args c++fmt-args)
