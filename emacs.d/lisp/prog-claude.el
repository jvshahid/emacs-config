;; There are currently two ways to interact with Claude code from Emacs, either the agent-shell which uses comint, or claude-emacs which starts claude in eat/terminal and interacts with it.  I'm still trying them both to see which one works best for me
(setenv "CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS" "1")
(straight-use-package 'claude-code-ide)
(global-set-key (kbd "C-c c") #'claude-code-ide-menu)
(with-eval-after-load 'claude-code-ide
 (setq claude-code-ide-terminal-backend 'eat)
 (setq claude-code-ide-cli-path "claude --dangerously-skip-permissions"))
