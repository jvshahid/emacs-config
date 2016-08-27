repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/direnv" do
  repository "git clone https://github.com/direnv/direnv"
  user ENV['SUDO_USER']
end

bash "build_direnv" do
  action :nothing
  cwd "#{repos_root}/direnv"
  code <<-EOF
    ln -s $PWD/direnv /home/$SUDO_USER/bin/
  EOF
end
