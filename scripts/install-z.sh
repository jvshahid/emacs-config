#!/usr/bin/bash -e

sudo pacman --noconfirm --needed -S curl

curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.z.sh
