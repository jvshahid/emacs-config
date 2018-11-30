#!/usr/bin/bash -e

cat <<PACKAGES | xargs pacman -S --noconfirm
i3lock
lightdm
lightdm-gtk-greeter
xf86-input-synaptics
xf86-video-intel
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
PACKAGES

systemctl enable lightdm

cp ../cookbooks/development/files/default/logind.conf /etc/systemd/logind.conf

mkdir -p /usr/share/xsessions
cp ../cookbooks/development/files/default/xinit.desktop /usr/share/xsessions/xinit.desktop

cp ../cookbooks/development/files/default/90-keyboard.hwdb /lib/udev/hwdb.d/90-keyboard.hwdb
udevadm hwdb --update
