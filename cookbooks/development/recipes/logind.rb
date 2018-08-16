directory "#{ENV['HOME']}/bin"

cookbook_file "/etc/systemd/logind.conf" do
  mode 0444
  owner "jvshahid"
end

