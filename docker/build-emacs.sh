#!/usr/bin/env bash

set -e

apt-get update
apt-get install -y git-core build-essential texinfo autoconf libgnutls28-dev libtinfo-dev silversearcher-ag ispell libgmime-2.6-dev libxapian-dev libxml2-dev
mkdir ~/.ssh
ssh-keyscan github.com >> ~/.ssh/known_hosts
git clone --depth 1 https://github.com/emacs-mirror/emacs
pushd emacs
  autoreconf -i
  ./configure --without-x
  make -j8
  make install
popd
rm -rf emacs
git clone https://github.com/jvshahid/emacs-config
ln -s /emacs-config/emacs.d ~/.emacs.d
emacs --batch -l ~/.emacs.d/init.el
apt-get remove -y --purge build-essential autoconf
apt-get autoremove -y
