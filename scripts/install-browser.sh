#!/usr/bin/bash -e

set -x

source ./helpers.sh

install_package brave-bin
install_package libu2f-host     # yubi key access from browser


