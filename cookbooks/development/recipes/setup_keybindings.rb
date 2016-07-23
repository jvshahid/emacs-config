bash "setup_keybinding" do
  user 'jvshahid'
  code <<EOF
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_left '<Primary>braceleft'
gconftool --type string --set /apps/gnome-terminal/keybindings/move_tab_right '<Primary>braceright'
gconftool --type string --set /apps/gnome-terminal/keybindings/next_tab '<Primary>bracketright'
gconftool --type string --set /apps/gnome-terminal/keybindings/prev_tab '<Primary>bracketleft'

dconf write /org/cinnamon/muffin/keybindings/move-to-workspace-right "['<Super>semicolon']"
dconf write /org/cinnamon/muffin/keybindings/move-to-workspace-left "['<Super>l']"
dconf write /org/cinnamon/muffin/keybindings/switch-to-workspace-left "['<Super>j']"
dconf write /org/cinnamon/muffin/keybindings/switch-to-workspace-right "['<Super>k']"

dconf write /org/cinnamon/keybindings/custom-list "['custom0']"
dconf write /org/cinnamon/keybindings/custom-keybindings/custom0/name "'diodon'"
dconf write /org/cinnamon/keybindings/custom-keybindings/custom0/binding "'<Control><Shift>V'"
dconf write /org/cinnamon/keybindings/custom-keybindings/custom0/command "'/usr/bin/diodon'"
EOF
end
