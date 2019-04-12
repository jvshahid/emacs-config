;;; -*- lexical-binding: t; -*-

(setq shell-file-name "/bin/bash")

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(with-eval-after-load 'tramp
  (push "~/.fzf/bin" tramp-remote-path)
  (push "~/bin" tramp-remote-path)
  (push 'tramp-own-remote-path tramp-remote-path))
