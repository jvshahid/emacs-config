#!/usr/bin/bash -e

[ ! -d ~/codez/direnv ] && git clone https://github.com/direnv/direnv ~/codez/direnv

pushd ~/codez/direnv
  git pull --rebase
  make install DESTDIR=/tmp/direnv
  mv /tmp/direnv/bin/direnv ~/bin/
popd
