#!/usr/bin/bash -e

systemctl enable gdm NetworkManager

cp ../dotfiles/90-keyboard.hwdb /lib/udev/hwdb.d/90-keyboard.hwdb
systemd-hwdb update
