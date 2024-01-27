#!/usr/bin/bash -e

source ./helpers.sh

install_package google-chrome

if [ ! -x /usr/share/applications/google-chrome.desktop.orig ]; then
    mv /usr/share/applications/google-chrome.desktop /usr/share/applications/google-chrome.desktop.orig
fi

sudo sed -i 's/\/usr\/bin\/google-chrome-stable/\/usr\/bin\/google-chrome-stable --ozone-platform-hint=auto/g' /usr/share/applications/google-chrome.desktop
