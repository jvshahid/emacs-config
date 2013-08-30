# get synergy
arch = node['kernel']['machine']
remote_filename = "http://synergy.googlecode.com/files/synergy-1.4.10-Linux-#{arch}.deb"
filename = 'synergy.deb'
remote_file filename do
  backup false
  source remote_filename
  use_last_modified true
end

package filename do
  provider Chef::Provider::Package::Dpkg
  source filename
end
