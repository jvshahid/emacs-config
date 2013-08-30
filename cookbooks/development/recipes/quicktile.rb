repos_root = File.expand_path "../../../../", File.dirname(__FILE__)

git "#{repos_root}/quicktile" do
  repository "https://github.com/ssokolow/quicktile"
  user "jvshahid"
end
