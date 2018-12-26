#!/usr/bin/bash -e

source ./helpers.sh

install_package gcc autoconf make gtk3 pkg-config git libtiff libxpm giflib libjpeg-turbo

[ ! -d ~/codez/emacs ] && git clone https://github.com/emacs-mirror/emacs.git ~/codez/emacs

pushd ~/codez/emacs
  [ ! -f ./configure ] && autoreconf -i
  [ ! -f Makefile ] && ./configure --prefix=$HOME/bin/emacs-27
  make install
popd
