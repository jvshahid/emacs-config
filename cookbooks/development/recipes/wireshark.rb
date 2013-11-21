# bash "fix wireshark permissions" do
#   code <<-EOF
#    dpkg-reconfigure -u wireshark-common
#   EOF
# end

group "wireshark" do
  action :modify
  members "jvshahid"
  append true
end

group "vboxusers" do
  action :modify
  members "jvshahid"
  append true
end

puts "                 *** IMPORTANT ***                        "
puts "Don't forget to run 'dpkg-reconfigure -u wireshark-common'"
