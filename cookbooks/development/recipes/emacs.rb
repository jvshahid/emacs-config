link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
end
