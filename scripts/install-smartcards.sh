#!/usr/bin/bash -e

sudo pacman --noconfirm --needed -S ccid
sudo systemctl enable pcscd.service
