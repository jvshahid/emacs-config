git "#{ENV["HOME"]}/.fzf" do
  repository "https://github.com/junegunn/fzf.git"
  user ENV['SUDO_USER']
  notifies :run, "bash[install_fzf]"
end

bash "install_fzf" do
  action :nothing
  cwd "#{ENV["HOME"]}/.fzf"
  code <<-EOF
    ./install --all || true
  EOF
end
