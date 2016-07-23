#
# Cookbook Name:: development
# Recipe:: default
#
# Copyright 2013, YOUR_COMPANY_NAME
#
# All rights reserved - Do Not Redistribute
#

include_recipe "development::emacs"

# exit early if we're not on linux
return if node[:os] != 'linux'

include_recipe "development::remove_avahi"
include_recipe "development::chrome"
include_recipe "development::clang"
include_recipe "development::arduino"
include_recipe "development::basic"
include_recipe "development::fzf"
include_recipe "development::autoraise"
include_recipe "development::packages"
include_recipe "development::pianobar"
include_recipe "development::cask"
include_recipe "development::ctags"
include_recipe "development::quicktile"
include_recipe "development::wireshark"
include_recipe "development::setup_keybindings"

# # run apt-get upgrade to get the latest packages
# bash "upgrad" do
#   code <<-EOF
#     apt-get -y upgrade
#   EOF
# end
