repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/pianobar" do
  revision '34b6068d229efcb934fa40b5f4d47a46cedae552'
  repository "https://github.com/PromyLOPh/pianobar.git"
  user ENV['SUDO_USER']
  notifies :run, "bash[build_pianobar]"
end

bash "build_pianobar" do
  action :nothing
  cwd "#{repos_root}/pianobar"
  code <<-EOF
    make
  EOF
end
