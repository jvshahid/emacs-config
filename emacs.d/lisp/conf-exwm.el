;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (define-key key-translation-map (kbd "C-8") (kbd "DEL"))
  (server-start)
  (load-theme 'just-grey t)
  (straight-use-package '(exwm :type git :host github :repo "ch11ng/exwm"))
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (require 'exwm-config)
  (exwm-config-default)

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
  (setq exwm-randr-workspace-monitor-plist '(0 "DP-1"))
  (defun shahid/exwm-randr-screen-changed ()
    (interactive)
    (if (= 0 (call-process-shell-command "xrandr | grep --silent 'DP-1 disconnected'"))
        (progn
          (golden-ratio-mode 1)
          (start-process-shell-command
           ;; xrandr --output <something> --same-as <other-thing> for mirroring
           "xrandr" nil "xrandr --output DP-1 --off --output LVDS-1 --auto"))
      (golden-ratio-mode -1)
      (start-process-shell-command
       ;; xrandr --output <something> --same-as <other-thing> for mirroring
       "xrandr" nil "xrandr --output DP-1 --above LVDS-1 --auto --output LVDS-1 --off")))
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

  (defun shahid/swap-monitors ()
    (interactive)
    (let ((new-pos (mod (1+ exwm-workspace-current-index) 2)))
      (exwm-workspace-move exwm-workspace--current new-pos)
      ;; make sure the right window is selected and has the focus
      (exwm-workspace-switch new-pos)
      (select-window (get-buffer-window))))

  (defun shahid/window-swap-monitors ()
    (interactive)
    (let ((new-pos (mod (1+ exwm-workspace-current-index) 2)))
      (exwm-workspace-move-window new-pos)
      ;; make sure the right window is selected and has the focus
      (exwm-workspace-switch new-pos)
      (select-window (get-buffer-window))))

  (shahid/bind-global-key "s-s" #'shahid/swap-monitors)
  (shahid/bind-global-key "s-w" #'shahid/window-swap-monitors)

  (shahid/bind-global-key "<XF86AudioMute>" #'toggle-audio-mute)
  (shahid/bind-global-key "<XF86AudioMicMute>" #'toggle-mic-mute)
  (shahid/bind-global-key "<XF86AudioLowerVolume>" #'lower-audio-volume)
  (shahid/bind-global-key "<XF86AudioRaiseVolume>" #'raise-audio-volume)
  (shahid/bind-global-key "C-s-l" #'lock-screen))

