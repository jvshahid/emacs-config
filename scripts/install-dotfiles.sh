#!/usr/bin/bash -e

repo=$(cd $(dirname $0)/.. && pwd)

ln -sf $repo/dotfiles/.ackrc ~/
ln -sf $repo/dotfiles/.gdbinit ~/
ln -sf $repo/dotfiles/.zshrc ~/
ln -sf $repo/dotfiles/.bashrc ~/
ln -sf $repo/dotfiles/.gemrc ~/
ln -sf $repo/dotfiles/.gitconfig ~/
ln -sf $repo/dotfiles/.gitignore_global ~/
ln -sf $repo/dotfiles/.clang-format ~/
ln -sf $repo/dotfiles/alias.sh ~/
ln -sf $repo/dotfiles/functions.sh ~/
ln -sf $repo/dotfiles/exports.sh ~/
ln -sf $repo/dotfiles/prompt.sh ~/
ln -sf $repo/dotfiles/.mbsyncrc ~/
ln -sf $repo/dotfiles/read-password-emacs ~/bin/

# these two need to be copied
cp -f $repo/dotfiles/.xinitrc ~/
chmod +x ~/.xinitrc
cp -f $repo/dotfiles/.Xresources ~/
