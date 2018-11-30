#!/usr/bin/bash

pacman -S --needed --noconfirm grub
mkdir -p /boot/grub
cd /boot/grub
grub-mkconfig > grub.cfg

device=$(df -h / | grep 'dev' | awk '{print $1}')
grub-install ${device/[0-9]/}
