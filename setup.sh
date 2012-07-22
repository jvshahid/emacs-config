#!/usr/bin/env bash

CONFIG_REPO=$(readlink -f $(dirname $0))
REPOS_DIR="$HOME/Documents/git"

echo "Repo root: $CONFIG_REPO"

if ! grep -R http://ppa.launchpad.net/cassou/emacs/ubuntu /etc/apt/sources.list.d/ > /dev/null 2>&1; then
    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
fi
[ -a ~/.xmodmap ] || ln -s $CONFIG_REPO/`hostname`.xmodmap ~/.xmodmap
[ -d ~/.emacs.d ] || ln -s $CONFIG_REPO ~/.emacs.d
[ -f ~/.emacs   ] || ln -s $CONFIG_REPO/.emacs ~/.emacs

if ! grep 'DO NOT REPLACE' ~/.bashrc > /dev/null 2>&1; then
    ln -s $CONFIG_REPO/.bashrc ~/.bashrc
fi

sudo apt-get install \
    emacs-snapshot-el \
    emacs-snapshot-gtk \
    emacs-snapshot \
    gnome-do \
    curl \
    openjdk-7-jdk

# TODO: change the capslock to control pragmatically
# TODO: change the window focus to follow the mouse pragmatically

# setup the ensime git repo
cd $REPOS_DIR
[ -d ensime ] || git clone git@github.com:jvshahid/ensime.git
cd ensime
[ -f ./sbt ] || curl 'https://raw.github.com/paulp/sbt-extras/master/sbt' -o ./sbt
chmod a+x ./sbt
# [ -d target/dist ] || ./sbt dist
# ln -s $REPOS_DIR/ensime $HOME/.emacs.d/libs/ensime_head
