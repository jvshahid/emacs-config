;; -*- lexical-binding: t; -*-

(straight-use-package 'go-mode)
(straight-use-package 'go-guru)
(straight-use-package 'go-eldoc)
(straight-use-package 'company-go)
(straight-use-package 'go-rename)
(straight-use-package '(ginkgo-mode :type git :host github :repo "jvshahid/ginkgo-mode" :branch "minor-fixes"))

(with-eval-after-load 'flycheck
  (setq flycheck-go-build-install-deps t))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (setq flycheck-disabled-checkers '(go-megacheck))))

;; disable the use of CGO inside emacs. otherwise, it complains that gcc is
;; missing while flychecking a buffer
(setenv "CGO_ENABLED" "0")

(with-eval-after-load 'go-mode
  (setq company-go-show-annotation t)

  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (require 'ginkgo-mode)
  (add-hook 'go-mode-hook 'ginkgo-mode)

  (setq ginkgo-use-pwd-as-test-dir t)
  (setq ginkgo-use-default-keys t)

  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-x 4 .") 'godef-jump-other-window)
  (define-key go-mode-map (kbd "M-?") 'go-guru-referrers)

  (add-hook 'go-mode-hook 'subword-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'go-mode-hook 'hs-minor-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local company-backends '(company-go)))))

(defun setup-gopath ()
  (when (and (eq (current-buffer) (window-buffer)) ; filter temp buffer events
             (equal major-mode 'go-mode)
             ;; ensure the buffer has a backing file
             buffer-file-name)
    (unless (boundp 'gopath)
      (setq-local gopath  nil)          ;prevent recursive calls
      (when-let ((root (locate-dominating-file buffer-file-name ".envrc")))
        (setq-local gopath (expand-file-name root))))
    (save-match-data
      (setenv "GOPATH" gopath))))

(add-hook 'buffer-list-update-hook #'setup-gopath)

(defun add-to-path (path)
  (let ((path (substitute-env-vars path)))
    (setq exec-path (cons path exec-path))
    (setenv "PATH" (concat path ":$PATH") t)))

(defun setup-goroot ()
  "Setup the golang environment, this function will install
   goimports, godef, godoc and gocode"
  (interactive)
  (let* ((goroot (expand-file-name (read-directory-name "GOROOT")))
         (gobin (concat goroot "/bin")))
    (add-to-path gobin)
    (setenv "GOROOT" goroot)))

(add-to-path "$HOME/.emacs.d/go/bin")

(defun install-go-deps ()
  (interactive)
  (let ((process-environment (cons (substitute-env-vars "GOPATH=$HOME/.emacs.d/go")
                                   process-environment))
        ;; make sure the buffer-list-update-hook don't run, otherwise it could
        ;; mess up the GOPATH environment variables
        (buffer-list-update-hook nil)
        (urls  '("golang.org/x/tools/cmd/goimports"
                 "github.com/rogpeppe/godef"
                 "github.com/nsf/gocode"
                 "github.com/dougm/goflymake"
                 "golang.org/x/tools/cmd/gorename"
                 "golang.org/x/tools/cmd/godoc"
                 "golang.org/x/lint/golint"
                 "github.com/kisielk/errcheck"
                 "github.com/mdempsky/unconvert"
                 "golang.org/x/tools/cmd/guru")))
    (let ((default-directory (substitute-env-vars "$HOME/.emacs.d/go")))
      (apply 'start-process "go-get" "*go-get*" "go" "get" "-u" urls))))

(setq gofmt-command "goimports")
(setq gofmt-args nil)

(global-set-key (kbd "C-c C-x d") 'godoc)

