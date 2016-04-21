bash "add chrome ppa" do
  code <<-EOF
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
    sudo apt-get update
  EOF
  not_if "grep -R chrome /etc/apt/sources.list.d/ > /dev/null 2>&1"
end
