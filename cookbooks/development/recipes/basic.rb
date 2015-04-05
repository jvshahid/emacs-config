directory "#{ENV['HOME']}/bin"

files = ['alias.sh', 'functions.sh', 'exports.sh', 'prompt.sh',
         '.ackrc', '.gdbinit', '.zshrc', 'bin/generate_tags.sh',
         'bin/cleanup_shit.sh', 'bin/setup_go_project.sh', '.emacs',
         '.gitconfig']

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

return if node[:development][:xmodmap].nil?

cookbook_file node[:development][:xmodmap] do
  path "#{ENV['HOME']}/.Xmodmap"
  mode 0444
  action :create
end
