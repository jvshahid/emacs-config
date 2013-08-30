hostname = `hostname`.strip.downcase

default[:development][:lenovo] = false

if hostname == 'thoth'
  default[:development][:lenovo] = true
end
