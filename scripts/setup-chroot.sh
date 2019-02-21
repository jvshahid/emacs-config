#!/usr/bin/bash -e

# this script is ran during system bootstrap inside the new root

cd $(dirname $0)

source ./helpers.sh

./setup-basic.sh

./setup-user.sh

./setup-grub.sh

install_package networkmanager networkmanager-openvpn
systemctl enable NetworkManager

./setup-window-manager.sh

./setup-fonts.sh

./setup-time-sync.sh

./setup-trackpoint-speed.sh

su jvshahid ./install-all.sh
