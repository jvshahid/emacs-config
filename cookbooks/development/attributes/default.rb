default[:development][:lenovo] = false
default[:development][:xmodmap] = nil
default[:arduino] = "#{ENV['HOME']}/Downloads/arduino.tgz"
default[:arduino_dir] = "#{ENV['HOME']}/bin/arduino"

def hostname
  node[:hostname]
end

if hostname == 'thoth'
  default[:development][:lenovo] = true
end

if hostname == 'amun' || hostname == 'horus'
  default[:development][:xmodmap] = "#{hostname}.xmodmap"
end
