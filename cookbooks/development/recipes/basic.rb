directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh',
  '.ackrc', '.gdbinit', '.zshrc', '.gemrc', 'bin/generate_tags.sh',
  'bin/flavor', 'bin/cleanup_shit.sh', 'bin/setup_go_project.sh',
  '.gitconfig', '.gitignore_global', '.clang-format', 'bin/diff-highlight',
  '.tmux.conf', '.mbsyncrc']

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

