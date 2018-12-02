#!/usr/bin/bash -e

source ./helpers.sh

packages=(
    acpi
    alsa-utils
    automake
    dstat
    ispell
    keepassx2
    libusb-compat
    libyaml
    openssh
    openssl
    pulseaudio
    pulseaudio-alsa
    pulseaudio-zeroconf
    rsync
    the_silver_searcher
    the_silver_searcher
    thermald
)

install_package ${packages[*]}

systemctl enable thermald.service
