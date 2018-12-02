#!/usr/bin/bash

# this script is ran during system bootstrap inside the new root

cd $(dirname $0)

./setup-basic.sh

./setup-user.sh

./setup-grub.sh

pacman -S --noconfirm networkmanager networkmanager-openvpn
systemctl enable NetworkManager

./setup-window-manager.sh

./setup-fonts.sh

./setup-time-sync.sh

pacman -S --needed --noconfirm openssh \
       libusb-compat \
       the_silver_searcher \
       openssl \
       libyaml \
       automake \
       keepassx2 \
       pulseaudio \
       pulseaudio-zeroconf \
       pulseaudio-alsa \
       alsa-utils \
       acpi \
       thermald

systemctl enable thermald.service

su jvshahid ./install-all.sh
