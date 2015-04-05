bash "fix auto raise" do
  # set focus follow mouse
  code <<-EOF
     gsettings set org.gnome.desktop.wm.preferences auto-raise true
  EOF
end
