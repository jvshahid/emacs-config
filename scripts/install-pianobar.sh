#!/usr/bin/bash -e

source ./helpers.sh

install_package ffmpeg libao

[ ! -d ~/codez/pianobar ] && git clone https://github.com/PromyLOPh/pianobar.git ~/codez/pianobar

pushd ~/codez/pianobar
  git pull --rebase
  make
popd

cat <<EOF | sudo tee /etc/libao.conf
default_driver=pulse
quiet
EOF
