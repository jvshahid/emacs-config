#!/usr/bin/env bash

cd `pwd $0`

set -e

# TODO: add-apt-repository doesn't seem to work without standard input attached
# to a tty, e.g. `add-apt-repository -y ppa:git-core/ppa </dev/null` doesn't
# work
if ! grep -R git /etc/apt/sources.list.d/ > /dev/null 2>&1; then
    sudo add-apt-repository -y ppa:git-core/ppa
    sudo apt-get update
fi

# install curl if it doesn't exist
which curl > /dev/null 2>&1 || sudo apt-get install curl

ruby_version=2.2.3
if ! which rvm > /dev/null 2>&1; then
    # install rvm
    curl -L https://get.rvm.io | bash -s stable
    source ~/.rvm/scripts/rvm
    rvm install $ruby_version
    rvm use $ruby_version
fi

if ! which bundle; then
    gem install bundler
fi

bundle install
rvmsudo -E bundle exec chef-solo -c solo.rb -j solo.json
echo "==========================================================================================="
echo "IMPORTANT: You will have to logout and log back in for wireshark permissions to take effect"
echo "==========================================================================================="

echo
echo
echo "===================================================================================="
echo "don't forgot to make the following changes by hand (they aren't automated yet)"

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
