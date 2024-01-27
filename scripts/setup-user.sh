#!/usr/bin/bash -e

source ./helpers.sh

groupadd jvshahid --gid 1000
groupadd sudo
useradd jvshahid -G sudo --uid 1000 -g jvshahid --no-create-home --shell /bin/zsh

echo "################ setting up password ################"
passwd jvshahid

cat >> /etc/sudoers <<SUDOERS
%sudo	ALL=(ALL:ALL) ALL
SUDOERS
