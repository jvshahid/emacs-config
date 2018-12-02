#!/usr/bin/bash -e

source ./helpers.sh

# this is the first time to run pacman. refresh the cache
pacman -Sy --noconfirm

install_package sudo zsh
groupadd jvshahid --gid 1000
groupadd sudo
useradd jvshahid -G sudo --uid 1000 -g jvshahid --no-create-home --shell /bin/zsh

echo "################ setting up password ################"
passwd jvshahid

cat >> /etc/sudoers <<SUDOERS
%sudo	ALL=(ALL:ALL) ALL
SUDOERS


