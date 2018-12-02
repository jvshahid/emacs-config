#!/usr/bin/bash

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

packages=(
    acpi
    alsa-utils
    automake
    keepassx2
    libusb-compat
    libyaml
    openssh
    openssl
    pulseaudio
    pulseaudio-alsa
    pulseaudio-zeroconf
    the_silver_searcher
    thermald
)

install_package ${packages[*]}

systemctl enable thermald.service

su jvshahid ./install-all.sh
