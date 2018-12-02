#!/usr/bin/bash

source ./helpers.sh

install_package gcc autoconf make gtk3 pkg-config git

[ ! -d ~/codez/emacs ] && git clone https://github.com/emacs-mirror/emacs.git ~/codez/emacs

pushd ~/codez/emacs
  autoreconf -i
  ./configure --prefix=$HOME/bin/emacs-27\
              --with-xpm=no\
              --with-jpeg=no\
              --with-gif=no\
              --with-tiff=no
  make install
popd
