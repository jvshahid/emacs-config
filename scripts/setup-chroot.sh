#!/usr/bin/bash -e

# this script is ran during system bootstrap inside the new root

cd $(dirname $0)

./setup-basic.sh

./setup-user.sh

./setup-grub.sh

./setup-window-manager.sh

./setup-time-sync.sh
