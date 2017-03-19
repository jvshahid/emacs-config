bash "setup_keybinding" do
  user 'jvshahid'
  code <<EOF
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_left '<Primary>braceleft'
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_right '<Primary>braceright'
gconftool --type string --set /apps/gnome-terminal/keybindings/next_tab '<Primary>bracketright'
gconftool --type string --set /apps/gnome-terminal/keybindings/prev_tab '<Primary>bracketleft'

# setup custom keybindings to paste using diodon
dconf write /org/cinnamon/desktop/keybindings/custom-list "['custom0']"
dconf write /org/cinnamon/desktop/keybindings/custom-keybindings/custom0/name "'diodon'"
dconf write /org/cinnamon/desktop/keybindings/custom-keybindings/custom0/command "'/usr/bin/diodon'"
dconf write /org/cinnamon/desktop/keybindings/custom-keybindings/custom0/binding "['<Primary><Shift>V']"

# setup keybindings
dconf write /org/cinnamon/desktop/keybindings/wm/move-to-workspace-right "['<Super>semicolon']"
dconf write /org/cinnamon/desktop/keybindings/wm/move-to-workspace-left "['<Super>l']"
dconf write /org/cinnamon/desktop/keybindings/wm/switch-to-workspace-left "['<Super>j']"
dconf write /org/cinnamon/desktop/keybindings/wm/switch-to-workspace-right "['<Super>k']"
dconf write /org/cinnamon/desktop/keybindings/looking-glass-keybinding '[""]'

# disable sound effects
dconf write /org/cinnamon/sounds/login-enabled false
dconf write /org/cinnamon/sounds/logout-enabled false
dconf write /org/cinnamon/sounds/unplug-enabled false
dconf write /org/cinnamon/sounds/tile-enabled false
dconf write /org/cinnamon/sounds/plug-enabled false
dconf write /org/cinnamon/sounds/switch-enabled false

EOF
end
