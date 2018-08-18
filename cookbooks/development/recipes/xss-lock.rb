repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/xss-lock" do
  revision '7b0b4dc83ff3716fd3051e6abf9709ddc434e985'
  repository "https://github.com/xdbob/xss-lock.git"
  user ENV['SUDO_USER']
  notifies :run, "bash[build_xss_lock]"
end

bash "build_xss_lock" do
  action :nothing
  cwd "#{repos_root}/xss-lock"
  code <<-EOF
    cmake . && make DESTDIR=$HOME/bin/xss-lock install
  EOF
end
