repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/pianobar" do
  repository "https://github.com/PromyLOPh/pianobar.git"
  user "jvshahid"
  notifies :run, "bash[build_pianobar]"
end

bash "build_pianobar" do
  action :nothing
  code <<-EOF
    make
  EOF
end
