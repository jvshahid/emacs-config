link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
end

repos_root = "#{ENV['HOME']}/codez"

git "#{repos_root}/emacs" do
  repository "https://github.com/emacs-mirror/emacs.git"
  user ENV['SUDO_USER']
  branch "emacs-25.2"
  notifies :run, "bash[build_emacs]"
end

bash "build_emacs" do
  action :nothing
  cwd "#{repos_root}/emacs"
  code <<-EOF
    autoreconf
    ./configure --prefix=$HOME/bin/emacs-25 --without-x
    make -j4 install
  EOF
end
