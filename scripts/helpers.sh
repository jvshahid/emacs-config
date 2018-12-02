#!/usr/bin/bash -e

function install_package() {
    pacman -S --needed --noconfirm $*
}

function install_aur_package() {
    name=$1
    url=https://aur.archlinux.org/$name.git
    dir=~/codez/aur/$name
    [ ! -d $dir ] && git clone $url $dir
    pushd $dir
      makepkg --noconfirm --needed -csi
    popd
}
