#!/usr/bin/bash -e

source ./helpers.sh

install_package patch

gpg --recv-key 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
install_aur_package dropbox
install_aur_package dropbox-cli
