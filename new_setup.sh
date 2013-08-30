#!/usr/bin/env bash

cd `pwd $0`

if ! which rvm > /dev/null 2>&1; then
    # install rvm
    curl -L https://get.rvm.io | bash -s stable
    source ~/.rvm/scripts/rvm
    rvm install 1.9.3
    source .rvmrc
fi

bundle install
chef-solo -c solo.rb -j solo.json
