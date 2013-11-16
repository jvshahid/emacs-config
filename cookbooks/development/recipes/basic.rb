hostname = `hostname`.strip
cookbook_file "#{ENV['HOME']}/.Xmodmap" do
  source "#{hostname}.xmodmap"
  mode 0444
  force_unlink true
end if File.exists?("#{hostname}.xmodmap")

directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh', '.ackrc', '.gdbinit', '.zshrc', 'bin/generate_tags.sh',
         'bin/cleanup_shit.sh', 'bin/setup_go_project.sh', '.emacs', '.gitconfig']
files.each do |file|
  cookbook_file "#{ENV['HOME']}/#{file}" do
    if file =~ /bin/
      mode 0544
    else
      mode 0444
    end
    owner "jvshahid"
  end
end

bash "fix auto raise" do
  code <<-EOF
     gsettings set org.gnome.desktop.wm.preferences auto-raise true
  EOF
end
