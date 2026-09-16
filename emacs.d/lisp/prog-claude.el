(straight-use-package '(acp :repo "xenodium/acp.el" :host github))
(straight-use-package '(agent-shell :repo "xenodium/agent-shell" :host github))
(straight-use-package '(agent-shell-manager :repo "jvshahid/agent-shell-manager" :host github))

;; --- agent-shell + pi integration ---
(with-eval-after-load 'agent-shell
  ;; Make pi the default for `M-x agent-shell'
  (setq agent-shell-preferred-agent-config
        (agent-shell-pi-make-agent-config))

  ;; Prefer resuming existing sessions
  (setq agent-shell-prefer-session-resume t
        agent-shell-session-strategy 'prompt)

  ;; Automatically send current file + region when sending code
  (setq agent-shell-context-sources '(files region error line)))

;; --- agent-shell end ---
