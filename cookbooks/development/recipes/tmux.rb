directory "#{ENV['HOME']}/.tmux/plugins" do
  recursive true
  user ENV['SUDO_USER']
end

git "#{ENV['HOME']}/.tmux/plugins/tpm" do
  repository "https://github.com/tmux-plugins/tpm"
  user ENV['SUDO_USER']
end

