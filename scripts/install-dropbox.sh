#!/usr/bin/env bash

sudo pacman -S --needed --noconfirm patch

source ./install-aur-package.sh

install_aur_package dropbox
install_aur_package dropbox-cli
