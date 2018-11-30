#!/usr/bin/bash -e

[ ! -d ~/.fzf ] && git clone https://github.com/junegunn/fzf.git ~/.fzf

pushd ~/.fzf
  git pull --rebase
  ./install --key-bindings --completion --no-update-rc
popd
