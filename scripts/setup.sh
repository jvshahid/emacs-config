#!/usr/bin/env bash

cd `pwd $0`

set -e

# install curl if it doesn't exist
which curl > /dev/null 2>&1 || sudo apt-get install curl

if ! which rvm > /dev/null 2>&1; then
    # install rvm
    curl -L https://get.rvm.io | bash -s stable
    source ~/.rvm/scripts/rvm
    rvm install 1.9.3
    source .rvmrc
fi

if ! which bundle; then
    gem install bundler
fi

bundle install
rvmsudo -E chef-solo -c solo.rb -j solo.json
echo "==========================================================================================="
echo "IMPORTANT: You will have to logout and log back in for wireshark permissions to take effect"
echo "==========================================================================================="

echo
echo
echo "===================================================================================="
echo "don't forgot to make the following changes by hand (they aren't automated yet)"

# TODO: change the capslock to control programatically
echo "* change the capslock to control or disable it"
# TODO: change the terminal key shortcuts programatically
echo "* change the terminal key shortcuts"
# gconftool-2 /apps/metacity/global_keybinndings/run_command_terminal -s '<Ctrl><Alt>t' -t string
# TODO: add the quicktile shortcuts programatically and change workspace switching shortcuts
echo "* add the quicktile shortcuts and change the workspace switching shortcuts"
# TODO: setup dropbox programatically
echo "* setup dropbox"

echo "===================================================================================="
echo
echo "Finished setting up the new machine, have fun hacking"
# setup the firewall

# Block access to 1.2.3.4 (vodafone image resolution modification proxy)
sudo iptables -A OUTPUT -p tcp -d 1.2.3.4 --dport 80 -j REJECT --reject-with tcp-reset

# finally clean up shit that I don't need
sudo apt-get -y autoremove
