directory "#{ENV['HOME']}/bin"

cookbook_file "/etc/systemd/logind.conf" do
  mode 0444
  owner "jvshahid"
end

cookbook_file "/etc/systemd/.xsession" do
  mode 0444
  owner "jvshahid"
end

cookbook_file "/usr/share/xsessions/xinit.desktop" do
  mode 0444
  owner "jvshahid"
end
