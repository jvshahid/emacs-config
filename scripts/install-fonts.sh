#!/usr/bin/bash -e

source ./helpers.sh

sudo pacman -S --needed --noconfirm ttf-dejavu ttf-ubuntu-font-family ttf-roboto

# nice arabic font
sudo pacman -S --needed --noconfirm fakeroot
install_aur_package ttf-amiri

