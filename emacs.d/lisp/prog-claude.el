;; There are currently two ways to interact with Claude code from Emacs, either the agent-shell which uses comint, or claude-emacs which starts claude in eat/terminal and interacts with it.  I'm still trying them both to see which one works best for me
(setenv "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS" "1")
(straight-use-package 'claude-code-ide)
(global-set-key (kbd "C-c c") #'claude-code-ide-menu)
(with-eval-after-load 'claude-code-ide
 (setq claude-code-ide-terminal-backend 'eat)
 ;; claude-code-ide does some stupid checks using call-process to detect
 ;; whether the cli is installed or not.  This doesn't work when the path
 ;; contains arguments like is the case here.  Solution: just override the
 ;; variable by setting it's value to true
 (setq claude-code-ide--cli-available t)
 (setq claude-code-ide-window-width 0.4)
 (setq claude-code-ide-cli-path "claude --dangerously-skip-permissions"))
 ;; (setq claude-code-ide-cli-path "/home/jvshahid/models/unsloth/Qwen3.5-35B-A3B-GGUF/start-claude.sh"))

(straight-use-package '(acp :repo "xenodium/acp.el" :host github))
(straight-use-package '(agent-shell :repo "xenodium/agent-shell" :host github))

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
