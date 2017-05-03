bash "setup_keybinding" do
  user 'jvshahid'
  code <<EOF
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_left '<Primary>braceleft'
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_right '<Primary>braceright'
gconftool --type string --set /apps/gnome-terminal/keybindings/next_tab '<Primary>bracketright'
gconftool --type string --set /apps/gnome-terminal/keybindings/prev_tab '<Primary>bracketleft'
gconftool --type string --set /apps/gnome-terminal/profiles/Default/background_type '"solid"'

# setup keybindings
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-1  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-2  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-3  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-4  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-5  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-6  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-7  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-8  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-9  "'disabled'"
dconf write /org/gnome/terminal/legacy/keybindings/switch-to-tab-10 "'disabled'"
dconf write /org/cinnamon/desktop/keybindings/wm/move-to-workspace-right "['<Super>semicolon']"
dconf write /org/cinnamon/desktop/keybindings/wm/move-to-workspace-left "['<Super>l']"
dconf write /org/cinnamon/desktop/keybindings/wm/switch-to-workspace-left "['<Super>j']"
dconf write /org/cinnamon/desktop/keybindings/wm/switch-to-workspace-right "['<Super>k']"
dconf write /org/cinnamon/desktop/keybindings/looking-glass-keybinding '[""]'

# disable sound effects
dconf write /org/cinnamon/desktop/sound/volume-sound-enabled false
dconf write /org/cinnamon/desktop/sound/event-sounds false
dconf write /org/cinnamon/sounds/login-enabled false
dconf write /org/cinnamon/sounds/logout-enabled false
dconf write /org/cinnamon/sounds/unplug-enabled false
dconf write /org/cinnamon/sounds/tile-enabled false
dconf write /org/cinnamon/sounds/plug-enabled false
dconf write /org/cinnamon/sounds/switch-enabled false

# enable mouse focus
dconf write /org/cinnamon/desktop/wm/preferences/auto-raise true
dconf write /org/cinnamon/desktop/wm/preferences/focus-mode '"mouse"'
dconf write /org/cinnamon/desktop/keybindings/wm/toggle-maximized '<Primary><Super>k'
dconf write /org/cinnamon/prevent-focus-stealing true

# disable touchpad
dconf write /org/cinnamon/settings-daemon/peripherals/touchpad/touchpad-enabled false

EOF
end
