#!/usr/bin/bash -e

source ./helpers.sh

install_package curl

curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.z.sh
