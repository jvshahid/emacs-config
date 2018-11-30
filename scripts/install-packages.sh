#!/usr/bin/bash -e

# rsync
cat <<PACKAGES | xargs sudo pacman --noconfirm --needed -S
dstat
rsync
PACKAGES

