link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
end

link "#{ENV['HOME']}/.emacs.d/libs/go" do
  to "#{ENV['HOME']}/bin/go/misc/emacs"
end

modes = {
  'git-modes'   => 'https://github.com/magit/git-modes.git',
  'magit-mode'  => 'https://github.com/magit/magit.git',
  'lua-mode'    => 'https://github.com/immerrr/lua-mode.git',
  'coffee-mode' => 'https://github.com/jvshahid/coffee-mode.git',
  'arduino-mode' => 'https://github.com/bookest/arduino-mode'
}

modes.each_pair do |name, url|
  git "#{ENV['HOME']}/.emacs.d/libs/#{name}" do
    repository url
    user ENV['USER']
  end
end

directory "#{ENV['HOME']}/.emacs.d/libs/go-autocomplete" do
  owner ENV['USER']
end

remote_filename = "https://raw.github.com/nsf/gocode/master/emacs/go-autocomplete.el"
filename = "#{ENV['HOME']}/.emacs.d/libs/go-autocomplete/go-autocomplete.el"
remote_file filename do
  backup false
  source remote_filename
  use_last_modified true
end

bash "build ensime" do
  # TODO: port this to ruby

  # [ -f ./sbt ] || curl 'https://raw.github.com/paulp/sbt-extras/master/sbt' -o ./sbt
  # chmod a+x ./sbt
  # [ -d target/dist ] || ./sbt stage
  # dist_dir=$(readlink -f $(ls -d dist*))
  # ln -s $dist_dir $HOME/.emacs.d/libs/ensime_head
  # popd

end
