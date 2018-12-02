#!/usr/bin/bash -e

source ./helpers.sh

install_package ccid
systemctl enable pcscd.service
