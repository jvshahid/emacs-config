#!/usr/bin/env bash

config_repo=$(readlink -f $(dirname $0))
repos_dir="$HOME/Documents/git"

if ! grep -R http://ppa.launchpad.net/cassou/emacs/ubuntu /etc/apt/sources.list.d/ > /dev/null 2>&1; then
    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
fi
[ -a ~/.xmodmap ] || ln -s $config_repo/`hostname`.xmodmap ~/.xmodmap
[ -d ~/.emacs.d ] || ln -s $config_repo ~/.emacs.d
[ -f ~/.emacs   ] || ln -s $config_repo/.emacs ~/.emacs

if ! grep 'DO NOT REPLACE' ~/.bashrc > /dev/null 2>&1; then
    ln -s $config_repo/.bashrc ~/.bashrc
fi

sudo apt-get install \
    emacs-snapshot-el \
    emacs-snapshot-gtk \
    emacs-snapshot \
    gnome-do \
    curl \
    openjdk-7-jdk \
    ack-grep \
    exuberant-ctags \
    htop \
    sysstat \
    dstat \
    iotop \
    ant \
    unrar \
    gcc \
    g++ \
    libtool \
    autoconf \
    automake \
    cmake \
    make \
    zlib1g-dev \
    libreadline6-dev \
    libssl-dev

# setup the ensime git repo
pushd $repos_dir
[ -d ensime ] || git clone git@github.com:jvshahid/ensime.git
pushd ensime
[ -f ./sbt ] || curl 'https://raw.github.com/paulp/sbt-extras/master/sbt' -o ./sbt
chmod a+x ./sbt
# [ -d target/dist ] || ./sbt dist
# ln -s $repos_dir/ensime $HOME/.emacs.d/libs/ensime_head
popd
popd


# setup the git aliases
[ -a $HOME/.gitconfig ] || ln -s $config_repo/.gitconfig $HOME/.gitconfig

# Setup rvm
if [ ! -d $HOME/.rvm ]; then
    curl -L https://get.rvm.io | bash -s stable --ruby
    source ~/.rvm/scripts/rvm
fi
( rvm list | grep 1.9.2 > /dev/null 2>&1 ) || rvm install 1.9.2
( rvm list | grep jruby-1.6.7 > /dev/null 2>&1 ) || rvm install jruby-1.6.7
( rvm list | grep jruby-head > /dev/null 2>&1 ) || rvm install jruby-head

# setup quicktile
if [ ! -d $repos_dir/quicktile ]; then
    pushd $repos_dir
    git clone https://github.com/ssokolow/quicktile
    popd
fi

echo "don't forgot to make the following changes by hand (they aren't automated yet)"

# TODO: change the capslock to control programatically
echo "change the capslock to control programatically"
# TODO: change the window focus to follow the mouse programatically
echo "change the window focus to follow the mouse programatically"
# TODO: change the terminal key shortcuts programatically
echo "change the terminal key shortcuts programatically"
# gconftool-2 /apps/metacity/global_keybindings/run_command_terminal -s '<Ctrl><Alt>t' -t string
# TODO: add the quicktile shortcuts programatically
echo "add the quicktile shortcuts programatically"

echo "Finished setting up the new machine, have fun hacking"
