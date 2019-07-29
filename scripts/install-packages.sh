#!/usr/bin/bash -e

source ./helpers.sh

packages=(
    acpi
    alsa-utils
    automake
    dnsutils
    dstat
    gnome-screenshot
    ispell
    keepassx2
    libreoffice-fresh
    libusb-compat
    libyaml
    mlocate
    openssh
    openssl
    pulseaudio
    pulseaudio-alsa
    pulseaudio-zeroconf
    rsync
    the_silver_searcher
    the_silver_searcher
    thermald
    unzip
)

install_package ${packages[*]}

$sudo systemctl enable thermald.service

bash ./disable-mic-boost.sh
