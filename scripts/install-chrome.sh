#!/usr/bin/bash -e

set -x

source ./helpers.sh

install_aur_package google-chrome
install_package libu2f-host     # yubi key access from chrome
