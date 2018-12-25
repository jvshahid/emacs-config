;;; -*- lexical-binding: t; -*-

(straight-use-package 'rvm)
(straight-use-package 'robe)            ;auto completion for ruby

(eval-after-load 'robe
  '(push 'company-robe company-backends))

(add-to-list 'hs-special-modes-alist
              `(ruby-mode
                ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                ,(rx (or "}" "]" "end"))                       ; Block end
                ,(rx (or "#" "=begin"))                        ; Comment start
                ruby-forward-sexp nil))

(add-hook 'ruby-mode-hook (lambda ()
                            (hs-minor-mode)
                            (robe-mode)
                            (flycheck-mode)
                            (define-key robe-mode-map (kbd "M-.") nil)
                            (local-set-key (kbd "M-.") 'company-complete)
                            (local-set-key (kbd "C-c C-j") 'robe-jump)))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.*" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-hook 'ruby-mode-hook 'subword-mode)
