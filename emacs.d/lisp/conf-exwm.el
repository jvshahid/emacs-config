;;; -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (define-key key-translation-map (kbd "C-8") (kbd "DEL"))
  (server-start)
  (load-theme 'tango-dark t)
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

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "DP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               ;; xrandr --output <something> --same-as <other-thing> for mirroring
               "xrandr" nil "xrandr --output DP-1 --above LVDS-1 --auto")))
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


  (exwm-input-set-key (kbd "<XF86AudioMute>") #'toggle-audio-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'toggle-mic-mute)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'lower-audio-volume)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'raise-audio-volume)
  (exwm-input-set-key (kbd "C-s-l") #'lock-screen)

  (global-set-key (kbd "<XF86AudioMute>") #'toggle-audio-mute)
  (global-set-key (kbd "<XF86AudioMicMute>") #'toggle-mic-mute)
  (global-set-key (kbd "<XF86AudioLowerVolume>") #'lower-audio-volume)
  (global-set-key (kbd "<XF86AudioRaiseVolume>") #'raise-audio-volume)
  (global-set-key (kbd "C-s-l") #'lock-screen))

