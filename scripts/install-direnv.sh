#!/usr/bin/bash -e

[ ! -d ~/codez/direnv ] && git clone https://github.com/direnv/direnv ~/codez/direnv

pushd ~/codez/direnv
  git pull --rebase
  ln -sf $PWD/direnv /home/jvshahid/bin/
popd
