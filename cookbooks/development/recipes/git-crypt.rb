repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/git-crypt" do
  branch "master"
  repository "https://github.com/AGWA/git-crypt"
  user ENV['SUDO_USER']
  notifies :run, "bash[build_git_crypt]"
end

bash "build_git_crypt" do
  action :nothing
  cwd "#{repos_root}/git-crypt"
  code <<-EOF
    make install PREFIX=#{ENV['HOME']}/bin/git-crypt
  EOF
end
