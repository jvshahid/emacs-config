repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{ENV['HOME']}/.cask" do
  repository "https://github.com/cask/cask"
  user ENV['SUDO_USER']
end
