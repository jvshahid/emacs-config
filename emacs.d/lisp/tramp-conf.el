;;; -*- lexical-binding: t; -*-

(with-eval-after-load 'tramp
  (push "~/.fzf/bin" tramp-remote-path)
  (push "~/bin" tramp-remote-path))
