default[:development][:lenovo] = false
default[:development][:xmodmap] = nil

def hostname
  node[:hostname]
end

if hostname == 'thoth'
  default[:development][:lenovo] = true
end

if hostname == 'amun' || hostname == 'horus'
  default[:development][:xmodmap] = "#{hostname}.xmodmap"
end
