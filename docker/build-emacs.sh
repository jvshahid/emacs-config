#!/usr/bin/env bash

set -e

apt-get update
apt-get install -y git-core \
        wget \
        unzip \
        build-essential \
        texinfo \
        autoconf \
        libgnutls28-dev \
        libxml2-dev \
        libtinfo-dev

mkdir ~/.ssh
ssh-keyscan github.com >> ~/.ssh/known_hosts
sha=4a7e74fea687011ee81dcbb02294bccd99b3a05f
git clone https://github.com/jvshahid/emacs.git -b easy-mmode-clear-buffer-list --depth 1
pushd emacs
  autoreconf -i
  ./configure CFLAGS='-O0 -g3' --without-x
  make -j8
  make install
popd
# rm -rf emacs
apt-get remove -y --purge build-essential autoconf gcc gcc-5
apt-get autoremove -y
rm -rf /var/lib/apt/lists
