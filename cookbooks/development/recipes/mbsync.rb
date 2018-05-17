repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/mu" do
  branch "master"
  repository "https://github.com/djcb/mu"
  user ENV['SUDO_USER']
  notifies :run, "bash[build_mu]"
end

bash "build_mu" do
  action :nothing
  cwd "#{repos_root}/mu"
  code <<-EOF
    ./autogen.sh && \
    ./configure --prefix=#{ENV['HOME']}/bin/mu && \
    make && make install
  EOF
end

git "#{repos_root}/mbsync" do
  branch "master"
  repository "https://git.code.sf.net/p/isync/isync"
  user ENV['SUDO_USER']
  notifies :run, "bash[build_mbsync]"
end

bash "build_mbsync" do
  action :nothing
  cwd "#{repos_root}/mbsync"
  code <<-EOF
    ./autogen.sh && \
    ./configure --prefix=#{ENV['HOME']}/bin/isync && \
    make && make install
  EOF
end
