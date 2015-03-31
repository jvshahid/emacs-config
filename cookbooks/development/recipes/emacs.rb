link "#{ENV['HOME']}/.emacs.d" do
  to File.expand_path("../../../emacs.d", File.dirname(__FILE__))
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
