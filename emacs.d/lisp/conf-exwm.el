;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (straight-use-package 'atomic-chrome)

  (atomic-chrome-start-server)

  (server-start)
  (straight-use-package '(exwm :type git :host github :repo "ch11ng/exwm"))
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (require 'exwm-config)
  (exwm-config-default)

  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 30) exwm-title
               (concat (substring exwm-title 0 29) "...")))))

  (setq exwm-input-simulation-keys `(([?\C-b] . [left])
                                     ([?\C-f] . [right])
                                     ([?\C-p] . [up])
                                     ([?\C-n] . [down])
                                     ([?\C-a] . [home])
                                     ([?\C-e] . [end])
                                     ([?\M-v] . [prior])
                                     ([?\C-v] . [next])
                                     ([?\C-d] . [delete])
                                     ([?\M-b] . [C-left])
                                     ([?\M-f] . [C-right])
                                     ([?\M-d] . [C-S-right C-x])
                                     (,(kbd "DEL") . [backspace])
                                     (,(kbd "C-8") . [backspace])
                                     ([?\C-/] . [C-z])
                                     ([?\C-m] . [return])
                                     ([?\M-<] . [C-home])
                                     ([?\M->] . [C-end])
                                     ([?\C-o] . [return left])
                                     ([?\C-k] . [S-end C-x delete])
                                     ([?\C-y] . [C-v])
                                     ([?\C-w] . [C-x])
                                     ([?\M-w] . [C-c])
                                     ([?\C-c ?\C-w] . [S-C-left C-x])
                                     ([?\C-  ?\C-n] . [S-down])
                                     ([?\C-  ?\C-p] . [S-up])
                                     ([?\C-  ?\M-f] . [S-C-right])
                                     ([?\C-  ?\M-b] . [S-C-left])
                                     ([?\C-  ?\C-a] . [S-home])
                                     ([?\C-  ?\C-e] . [S-end])
                                     ([?\C- ?\M-<] . [S-C-home])
                                     ([?\C- ?\M->] . [S-C-end])))

  (exwm-input--set-simulation-keys exwm-input-simulation-keys)

  (setq exwm-workspace-number 2
        exwm-workspace-switch-create-limit 0)

  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  (require 'exwm-randr)

  (defun shahid/exwm-randr-screen-changed ()
    (interactive)
    (if (= 0 (call-process-shell-command (format "xrandr | grep --silent '%s disconnected'" shahid/external-monitor-name)))
        (progn
          (start-process-shell-command
           ;; xrandr --output <something> --same-as <other-thing> for mirroring
           "xrandr" nil (format "xrandr --output %s --off --output LVDS-1 --auto" shahid/external-monitor-name)))
      (let ((cmd
             (cl-case shahid/monitor-config
               ('external (format "xrandr --output %s --above LVDS-1 --auto --output LVDS-1 --off" shahid/external-monitor-name))
               ('both (format "xrandr --output %s --above LVDS-1 --auto --output LVDS-1 --auto" shahid/external-monitor-name))
               ;; (format xrandr --output LVDS-1 --mode 1366x768 --output %s --mode 1366x768 --same-as LVDS-1 shahid/external-monitor-name)
               ('mirror (let ((mode (shahid/highest-resolution-for-mirror)))
                          (format "xrandr --output LVDS-1 --mode %s --output %s --mode %s --same-as LVDS-1" mode shahid/external-monitor-name mode))))))
        (start-process-shell-command
         "xrandr" nil cmd))))

  (defun shahid/change-display-configuration (sym value)
    (set sym value)
    (setq exwm-randr-workspace-monitor-plist `(0 ,shahid/external-monitor-name))
    (shahid/exwm-randr-screen-changed))

  (defcustom shahid/monitor-config 'external
    ""
    :type '(choice (const :tag "External monitor only" external)
                   (const :tag "Both laptop and External monitor" both)
                   (const :tag "Mirror laptop and External monitors" mirror))
    :initialize 'custom-initialize-default
    :set 'shahid/change-display-configuration)

  (defcustom shahid/external-monitor-name "DP-2"
    ""
    :type '(string)
    :set 'shahid/change-display-configuration)

  (defun shahid/highest-resolution-for-mirror ()
    (string-trim
     (shell-command-to-string
      "xrandr | grep --color=no -E '^ +[0-9]+x[0-9]+' | awk '{print $1}' | sort -nr | uniq -c | grep --color=no -E '^ +2' | head -1 | awk '{print $2}'")))

  (add-hook 'exwm-randr-screen-change-hook #'shahid/exwm-randr-screen-changed)

  (exwm-randr-enable)

  (defun toggle-mic-mute ()
    (interactive)
    (start-process "toggle-audio-mute" nil "amixer" "set" "Capture" "toggle"))

  (defun toggle-audio-mute ()
    (interactive)
    (start-process "toggle-audio-mute" nil "amixer" "-q" "sset" "Master" "1+" "toggle"))

  (defun raise-audio-volume ()
    (interactive)
    (start-process "raise-audio-volume" nil "amixer" "-q" "sset" "Master" "5%+"))

  (defun lock-screen ()
    (interactive)
    (start-process "lock-screen" nil "xset" "s" "activate"))

  (defun lower-audio-volume ()
    (interactive)
    (start-process "raise-audio-volume" nil "amixer" "-q" "sset" "Master" "5%-"))

  (defun shahid/to-workspace (inc)
    (let ((new-idx (+ exwm-workspace-current-index inc)))
      (exwm-workspace-switch new-idx)))

  (defun shahid/clean-dead-frames ()
    (interactive)
    (setq exwm-workspace--list (seq-filter (lambda (f)
                                             (frame-live-p f))
                                           exwm-workspace--list)))

  ;; override the original definition
  (defun shahid/bind-global-key (key func)
    (global-set-key (kbd key) func)
    (exwm-input-set-key (kbd key) func))

  (shahid/bind-global-key "s-," (lambda ()
                                  (interactive)
                                  (shahid/to-workspace -1)))
  (shahid/bind-global-key "s-." (lambda ()
                                  (interactive)
                                  (shahid/to-workspace 1)))
  (shahid/bind-global-key "<XF86AudioMute>" #'toggle-audio-mute)
  (shahid/bind-global-key "<XF86AudioMicMute>" #'toggle-mic-mute)
  (shahid/bind-global-key "<XF86AudioLowerVolume>" #'lower-audio-volume)
  (shahid/bind-global-key "<XF86AudioRaiseVolume>" #'raise-audio-volume)

  ;; Avoid EXWM grabbing other sequences starting with C-c.  That means the
  ;; following key bindings won't work in buffers that are in char-mode.
  (defun shahid/exwm-new (command)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))
  (shahid/bind-global-key "s-n" #'shahid/exwm-new)
  (shahid/bind-global-key "s-q" #'shahid/exwm-randr-screen-changed)
  (shahid/bind-global-key "s-l" #'lock-screen))

