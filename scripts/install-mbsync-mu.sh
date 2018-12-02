#!/usr/bin/bash -e

source ./helpers.sh

install_package gmime3 xapian-core

[ ! -d ~/codez/mu ] && git clone https://github.com/djcb/mu ~/codez/mu
pushd ~/codez/mu
  git pull --rebase
  [ ! -f ./configure ] && ./autogen.sh
  [ ! -f Makefile ] && ./configure --prefix=$HOME/bin/mu
  make install
popd

[ ! -d ~/codez/mbsync ] && git clone https://git.code.sf.net/p/isync/isync ~/codez/mbsync
pushd ~/codez/mbsync
  git pull --rebase
  [ ! -f ./configure ] && ./autogen.sh
  [ ! -f Makefile ] && ./configure --prefix=$HOME/bin/isync
  make install
popd
