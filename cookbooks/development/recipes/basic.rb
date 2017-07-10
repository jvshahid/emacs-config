directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh',
  '.ackrc', '.gdbinit', '.zshrc', '.gemrc', 'bin/generate_tags.sh',
  'bin/flavor', 'bin/cleanup_shit.sh', 'bin/setup_go_project.sh',
  '.gitconfig', '.gitignore_global', '.clang-format', 'bin/diff-highlight',
  '.tmux.conf']

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

cookbook_file "90-keyboard.hwdb" do
  path "/lib/udev/hwdb.d/90-keyboard.hwdb"
  mode 0644
  action :create
  notifies :run, "bash[rebuild_hwdb]"
end

bash "rebuild_hwdb" do
  code <<-EOF
   udevadm hwdb --update
EOF
end
