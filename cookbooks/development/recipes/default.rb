#
# Cookbook Name:: development
# Recipe:: default
#
# Copyright 2013, YOUR_COMPANY_NAME
#
# All rights reserved - Do Not Redistribute
#
bash "remove avahi" do
  code <<-EOF
    sudo apt-get -y remove avahi-daemon
  EOF
end
include_recipe "development::chrome"
include_recipe "development::basic"
include_recipe "development::packages"
include_recipe "development::pianobar"
include_recipe "development::ctags"
include_recipe "development::emacs"
include_recipe "development::go"
include_recipe "development::pianobar"
include_recipe "development::quicktile"
include_recipe "development::wireshark"

# # run apt-get upgrade to get the latest packages
# bash "upgrad" do
#   code <<-EOF
#     apt-get -y upgrade
#   EOF
# end
