#!/usr/bin/env bash

repo=$(cd $(dirname $0)/.. && pwd)

ln -sf $repo/cookbooks/development/files/default/.ackrc ~/
ln -sf $repo/cookbooks/development/files/default/.gdbinit ~/
ln -sf $repo/cookbooks/development/files/default/.zshrc ~/
ln -sf $repo/cookbooks/development/files/default/.gemrc ~/
ln -sf $repo/cookbooks/development/files/default/.gitconfig ~/
ln -sf $repo/cookbooks/development/files/default/.gitignore_global ~/
ln -sf $repo/cookbooks/development/files/default/.clang-format ~/
ln -sf $repo/cookbooks/development/files/default/alias.sh ~/
ln -sf $repo/cookbooks/development/files/default/functions.sh ~/
ln -sf $repo/cookbooks/development/files/default/exports.sh ~/
ln -sf $repo/cookbooks/development/files/default/prompt.sh ~/
ln -sf $repo/cookbooks/development/files/default/.mbsyncrc ~/
ln -sf $repo/cookbooks/development/files/default/.gnupg/gpg-agent.conf ~/

# these two need to be copied
cp -f $repo/cookbooks/development/files/default/.xinitrc ~/
chmod +x ~/.xinitrc
cp -f $repo/cookbooks/development/files/default/.Xresources ~/
