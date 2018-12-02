#!/usr/bin/bash -e

source ./helpers.sh

install_package gmime3 xapian-core

[ ! -d ~/codez/mu ] && git clone https://github.com/djcb/mu ~/codez/mu
pushd ~/codez/mu
  git pull --rebase
  ./autogen.sh
  ./configure --prefix=$HOME/bin/mu
  make
  make install
popd

[ ! -d ~/codez/mbsync ] && git clone https://git.code.sf.net/p/isync/isync ~/codez/mbsync
pushd ~/codez/mbsync
  git pull --rebase
  ./autogen.sh
  ./configure --prefix=$HOME/bin/isync
  make
  make install
popd
