#!/usr/bin/bash -e

cd $(dirname $0)

./install-yay.sh
#./install-direnv.sh
./install-browser.sh
./install-dotfiles.sh
#./install-dropbox.sh
#./install-fzf.sh
#./install-z.sh
#./install-gitcrypt.sh
#./install-mbsync-mu.sh
#./install-paman.sh
./install-fonts.sh
#./install-packages.sh
#./install-smartcards.sh
#./install-printer.sh
#./install-cups.sh
./install-rsyslog.sh
