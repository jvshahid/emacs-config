#!/usr/bin/bash -e

set -e

cd $(dirname $0)

echo -n "please enter device name (e.g. /dev/sda1): "
read device

mkfs.ext4 $device

# setup time
timedatectl set-ntp true

mkdir -p /mnt
mount $device /mnt
pacstrap /mnt base linux linux-firmware grub sudo zsh emacs-wayland wayland gnome networkmanager chrony fakeroot make gcc

genfstab -U /mnt > /mnt/etc/fstab
echo '/dev/disk/by-uuid/bdbcd19a-b7a5-4048-a8a0-12fe5c5ed640 /home/jvshahid auto nosuid,nodev,nofail,x-gvfs-show 0 0' >> /mnt/etc/fstab

mkdir -p /mnt/home/jvshahid
mount --bind /shahid /mnt/home/jvshahid
arch-chroot /mnt ${PWD/shahid/home\/jvshahid}/setup-chroot.sh
