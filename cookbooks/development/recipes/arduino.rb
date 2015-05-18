# remote_file node[:arduino] do
#   source "http://arduino.cc/download.php?f=/arduino-1.0.6-linux64.tgz"
# end

directory node[:arduino_dir] do
  owner 'jvshahid'
  group 'jvshahid'
end

bash "extract arduino" do
  code <<-EOF
    tar -C #{node[:arduino_dir]} --strip-components=1 -xvzf #{node[:arduino]}
EOF
end

git "#{node[:arduino_dir]}/Arduino-Makefile" do
  repository "https://github.com/sudar/Arduino-Makefile"
  user 'jvshahid'
  group 'jvshahid'
end
