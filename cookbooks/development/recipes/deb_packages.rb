# get synergy
arch = node['kernel']['machine']

files = [
  'http://synergy.googlecode.com/files/synergy-1.4.10-Linux-{arch}.deb'
]

files.each do |file|
  remote_filename = file.gsub(/{arch}/, arch)
  filename = Chef::Config[:file_cache_path] + "/" + File.basename(remote_filename)

  remote_file filename do
    backup false
    source remote_filename
    use_last_modified true
  end

  package filename do
    provider Chef::Provider::Package::Dpkg
    source filename
  end
end
