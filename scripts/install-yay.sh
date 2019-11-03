#!/usr/bin/bash -e

source ./helpers.sh

name=yay-git
url=https://aur.archlinux.org/$name.git
dir=~/codez/aur/$name
[ ! -d $dir ] && git clone $url $dir
pushd $dir
  makepkg --noconfirm --needed -csi
popd
