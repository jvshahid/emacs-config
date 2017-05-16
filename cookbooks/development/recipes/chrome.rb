path = "/etc/apt/sources.list.d/google-chrome.list"

bash "add chrome ppa" do
  code <<-EOF
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> #{path}'
    sudo apt-get update
  EOF
  not_if { ::File.exist?(path) }
end
