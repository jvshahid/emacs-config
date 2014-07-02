repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/ctags" do
  repository "https://github.com/jvshahid/ctags-exuberant.git"
  user "#{ENV['USER']}"
  notifies :run, "bash[build_ctags]"
end

bash "build_ctags" do
  action :nothing
  cwd "#{repos_root}/ctags"
  code <<-EOF
    autoreconf && ./configure --prefix=$HOME/bin/ctags && make -j4 && make install
  EOF
end
