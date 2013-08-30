cookbook_file "#{ENV['HOME']}/.Xmodmap" do
  hostname = `hostname`.strip
  source "#{hostname}.xmodmap"
  force_unlink true
end

directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh', '.ackrc', '.gdbinit', '.zshrc', 'bin/generate_tags.sh',
         'bin/cleanup_shit.sh', '.emacs', '.gitconfig']
files.each do |file|
  cookbook_file "#{ENV['HOME']}/#{file}" do
    mode 0755 if file =~ /bin/
    owner "jvshahid"
  end
end
