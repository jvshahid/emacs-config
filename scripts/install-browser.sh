#!/usr/bin/bash -e

set -x

source ./helpers.sh

installe_package firefox
install_package libu2f-host     # yubi key access from browser
xdg-mime default firefox.desktop x-scheme-handler/https
xdg-mime default firefox.desktop x-scheme-handler/http


