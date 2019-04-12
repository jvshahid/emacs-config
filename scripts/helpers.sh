#!/usr/bin/bash -e

function install_package() {
    yay -S --noconfirm $*
}

function install_aur_package() {
    yay -G --noconfirm $*
}

function yay_update_all() {
    yay -Syu --timeupdate --noconfirm
}
