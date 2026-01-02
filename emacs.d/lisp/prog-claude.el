;; There are currently two ways to interact with Claude code from Emacs, either the agent-shell which uses comint, or claude-emacs which starts claude in eat/terminal and interacts with it.  I'm still trying them both to see which one works best for me
(straight-use-package '(acp :type git
                            :host github
                            :repo  "xenodium/acp.el"))
(straight-use-package 'shell-maker)
(straight-use-package '(agent-shell :type git
                                    :host github
                                    :repo  "xenodium/agent-shell"))

(require 'agent-shell)

(defun claude ()
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (buffer (eat "claude" t)))
    (with-current-buffer buffer
      (rename-buffer "claude" t))))


(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
