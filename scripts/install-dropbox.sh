#!/usr/bin/env bash

sudo pacman -S --needed --noconfirm patch

source ./helpers.sh

install_aur_package dropbox
install_aur_package dropbox-cli
