#!/usr/bin/bash -e

source ./helpers.sh

install_package ffmpeg libao

[ ! -d ~/codez/pianobar ] && git clone https://github.com/PromyLOPh/pianobar.git ~/codez/pianobar

pushd ~/codez/pianobar
  # pianobar introduced buffering in the following commit which causes volume
  # changes to be delayed.  Use the previous commit to avoid that issue.
  git checkout bbbdd99fdaacfe0a3c9a1237fa0d086907c1286f~
  make
popd

cat <<EOF | sudo tee /etc/libao.conf
default_driver=pulse
quiet
EOF
