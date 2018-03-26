#!/usr/bin/env bash

set -e

apt-get update
apt-get install -y git-core \
        build-essential \
        texinfo \
        autoconf \
        libgnutls28-dev \
        libtinfo-dev \
        silversearcher-ag \
        ispell \
        libgmime-2.6-dev \
        libxapian-dev \
        libxml2-dev \
        libgtkextra-dev \
        openjdk-9-jdk-headless \
        libgtkextra-dev

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
apt-get remove -y --purge build-essential autoconf
apt-get autoremove -y
