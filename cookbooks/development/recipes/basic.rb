directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh',
  '.ackrc', '.gdbinit', '.zshrc', '.gemrc', 'bin/generate_tags.sh',
  'bin/flavor', 'bin/cleanup_shit.sh', 'bin/setup_go_project.sh', '.emacs',
  '.gitconfig', '.gitignore_global', '.clang-format']

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

cookbook_file "remap_keys" do
  path "/etc/init.d/remap_keys"
  mode 0755
  action :create
end
