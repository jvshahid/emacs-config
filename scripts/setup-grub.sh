#!/usr/bin/bash -e

source ./helpers.sh

mkdir -p /boot/grub
cd /boot/grub
grub-mkconfig -o grub.cfg

device=$(df -h / | grep 'dev' | awk '{print $1}')
grub-install ${device/[0-9]/}
