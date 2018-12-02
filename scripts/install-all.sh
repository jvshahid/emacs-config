#!/usr/bin/bash -e

cd $(dirname $0)

./install-emacs.sh

./install-java.sh
./install-chrome.sh
./install-direnv.sh
./install-dotfiles.sh
./install-dropbox.sh
./install-fzf.sh
./install-z.sh
./install-gitcrypt.sh
./install-pianobar.sh
./install-mbsync-mu.sh
./install-paman.sh
./install-fonts.sh
./install-packages.sh
./install-smartcards.sh
