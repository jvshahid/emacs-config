repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/mu" do
  revision 'e50e6bd3d1e896595e0be5487c3ea8ce237eb44f'
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
  revision 'cbac8aa75ce041384155d360f040aec5049d1de5'
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
