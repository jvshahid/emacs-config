link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
end

link "#{ENV['HOME']}/.emacs.d/libs/go" do
  to "#{ENV['HOME']}/bin/go/misc/emacs"
end

modes = {
  'forml-mode'  => 'git@github.com:jvshahid/forml-mode.git',
  'lua-mode'    => 'git://github.com/immerrr/lua-mode.git',
  'coffee-mode' => 'git@github.com:jvshahid/coffee-mode.git',
  'ensime-mode' => 'git@github.com:jvshahid/ensime.git',
}

modes.each_pair do |name, url|
  git "#{ENV['HOME']}/.emacs.d/libs/#{name}" do
    repository url
    user "jvshahid"
  end
end

directory "#{ENV['HOME']}/.emacs.d/libs/go-autocomplete" do
  owner "jvshahid"
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