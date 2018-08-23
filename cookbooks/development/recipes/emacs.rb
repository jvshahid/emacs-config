link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
end

repos_root = "#{ENV['HOME']}/codez"

git "#{repos_root}/emacs" do
  repository "https://github.com/emacs-mirror/emacs.git"
  user ENV['SUDO_USER']
  branch "master"
  notifies :run, "bash[build_emacs]"
end

bash "build_emacs" do
  action :nothing
  cwd "#{repos_root}/emacs"
  code <<-EOF
    autoreconf -i
    ./configure --prefix=$HOME/bin/emacs-27 --with-xpm=no --with-jpeg=no --with-gif=no --with-tiff=no
    make install
  EOF
end
