# bash "fix wireshark permissions" do
#   code <<-EOF
#    dpkg-reconfigure -u wireshark-common
#   EOF
# end

group "wireshark" do
  action :create
  members "jvshahid"
  append true
end

group "vboxusers" do
  action :create
  members "jvshahid"
  append true
end
