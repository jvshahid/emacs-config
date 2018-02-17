repos_root = "#{ENV['HOME']}/codez"

git "#{repos_root}/pinentry" do
  repository "https://github.com/gpg/pinentry.git"
  user ENV['SUDO_USER']
  branch "pinentry-1.1.0"
  notifies :run, "bash[build_pinentry]"
end

bash "build_pinentry" do
  action :nothing
  cwd "#{repos_root}/pinentry"
  code <<-EOF
    ./autogen.sh && \
    ./configure --enable-pinentry-emacs --enable-inside-emacs --enable-maintainer-mode --prefix=$HOME/bin/pinentry && \
    make install
  EOF
end
