#!/usr/bin/bash -e

source ./helpers.sh

packages=(
    i3lock
    unclutter
    lightdm
    lightdm-gtk-greeter
    xf86-input-synaptics
    xorg-server
    xorg-xhost
    xorg-xhost
    xorg-xrandr
    xorg-xrdb
    xorg-xset
    xorg-xset
    xorg-xsetroot
    xss-lock
    xss-lock
)

install_package ${packages[*]}

systemctl enable lightdm

cp ../dotfiles/logind.conf /etc/systemd/logind.conf

mkdir -p /usr/share/xsessions
cp ../dotfiles/xinit.desktop /usr/share/xsessions/xinit.desktop

cp ../dotfiles/90-keyboard.hwdb /lib/udev/hwdb.d/90-keyboard.hwdb
udevadm hwdb --update
