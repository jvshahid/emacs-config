#!/usr/bin/bash -e

source ./helpers.sh

install_package ccid

sudo systemctl enable pcscd.service
