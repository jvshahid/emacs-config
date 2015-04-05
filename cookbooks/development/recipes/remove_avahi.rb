bash "remove avahi" do
  code <<-EOF
    sudo apt-get -y remove avahi-daemon
  EOF
end
