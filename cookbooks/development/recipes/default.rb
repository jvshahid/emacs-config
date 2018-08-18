#
# Cookbook Name:: development
# Recipe:: default
#
# Copyright 2013, YOUR_COMPANY_NAME
#
# All rights reserved - Do Not Redistribute
#

# exit early if we're not on linux
return if node[:os] != 'linux'

# things that i need on any machine
include_recipe "development::packages"
include_recipe "development::tmux"
include_recipe "development::setup_desktop_environment"
include_recipe "development::z"
include_recipe "development::fzf"

# extras on my laptop
include_recipe "development::basic"
include_recipe "development::display-environment"
include_recipe "development::xss-lock"
include_recipe "development::remove_avahi"
include_recipe "development::chrome"
include_recipe "development::clang"
include_recipe "development::arduino"
include_recipe "development::autoraise"
include_recipe "development::pianobar"
include_recipe "development::ctags"
include_recipe "development::mbsync"
include_recipe "development::wireshark"

# # run apt-get upgrade to get the latest packages
# bash "upgrad" do
#   code <<-EOF
#     apt-get -y upgrade
#   EOF
# end
