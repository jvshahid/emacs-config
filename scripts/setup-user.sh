#!/usr/bin/bash

pacman -Sy --noconfirm
pacman -S --noconfirm sudo zsh
groupadd jvshahid --gid 1000
groupadd sudo
useradd jvshahid -G sudo --uid 1000 -g jvshahid --no-create-home --shell /bin/zsh

echo "################ setting up password ################"
passwd jvshahid

cat >> /etc/sudoers <<SUDOERS
%sudo	ALL=(ALL:ALL) ALL
SUDOERS


