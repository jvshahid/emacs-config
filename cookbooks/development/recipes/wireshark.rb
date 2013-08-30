bash "fix wireshark permissions" do
  code <<-EOF
   dpkg-reconfigure wireshark-common
  EOF
end

group "wireshark" do
  action :modify
  members "jvshahid"
  append true
end
