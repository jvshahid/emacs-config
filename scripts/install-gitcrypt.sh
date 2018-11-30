#!/usr/bin/bash -e

[ ! -d ~/codez/git-crypt ] && git clone https://github.com/AGWA/git-crypt ~/codez/git-crypt

pushd ~/codez/git-crypt
  git pull --rebase
  make install PREFIX=$HOME/bin/git-crypt
popd

