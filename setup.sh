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
    . ~/.bashrc
fi

sudo apt-get install \
    emacs-snapshot-el \
    emacs-snapshot-gtk \
    emacs-snapshot \
    gnome-do \
    curl \
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
    libreadline-dev \
    libssl-dev \
    sshfs \
    libjson0-dev \
    libfaad-dev \
    libmad0-dev \
    libgcrypt11-dev \
    libgnutls-dev \
    libao-dev \
    id3 \
    libxml2-dev \
    libxslt1-dev \
    samba \
    mercurial \
    xclip \
    colordiff \
    evince \
    mysql-server \
    mysql-client \
    libmysqlclient-dev \
    libsqlite3-dev \
    openssh-server \
    acpi \
    libprotobuf-dev \
    libncurses5-dev \
    libio-pty-perl \
    chmsee \
    ghc \
    ghc-doc

# this might fail on old distros
sudo apt-get install openjdk-7-jdk

# setup mosh
mosh_installation_dir=$(eval "echo $MOSH_INSTALLATION")
if [ ! -d $mosh_installation_dir ]; then
    pushd $repos_dir
    [ -d mosh ] || git clone https://github.com/keithw/mosh.git
    pushd mosh
    autoreconf -i
    ./configure --prefix=$mosh_installation_dir && make && make install
    popd
    popd
fi

if ! dpkg -l | grep vagrant; then
    filename=$(mktemp)
    if [ ! $? ]; then
        echo "cannot create tempfile ${filename}"
        exit 1
    fi
    echo "Using ${filename} as the temporary filename"
    curl 'http://files.vagrantup.com/packages/5ab18a4f114c2bcbcce67db40b18d026264f428c/vagrant_1.0.4_x86_64.deb' -o ${filename}
    if ! sudo dpkg -i ${filename}; then
        echo "Cannot install vagrant"
        exit 1
    fi
    rm $filename
fi

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
    rvm pkg install readline
fi
( rvm list | grep 1.9.3 > /dev/null 2>&1 ) || ( rvm install 1.9.3 && rvm use --default 1.9.3 )
( rvm list | grep jruby-1.6.7 > /dev/null 2>&1 ) || rvm install jruby-1.6.7
( rvm list | grep jruby-head > /dev/null 2>&1 ) || rvm install jruby-head

# setup pianobar
if [ ! -d $repos_dir/pianobar ]; then
    pushd $repos_dir
    git clone https://github.com/PromyLOPh/pianobar.git
    pushd pianobar
    make
    popd
    popd
fi

# create the generate tags script
if [ ! -f ~/Documents/generate_tags.sh ]; then
    cat > ~/Documents/generate_tags.sh <<EOF
find . -print0 -name *.cpp -or -name *.java -or -name *.rb | xargs --null etags
EOF
    chmod a+x ~/Documents/generate_tags.sh
fi

# setup quicktile
if [ ! -d $repos_dir/quicktile ]; then
    pushd $repos_dir
    git clone https://github.com/ssokolow/quicktile
    popd
fi

# TODO: setup mosh

echo
echo
echo "================================================================"
echo "don't forgot to make the following changes by hand (they aren't automated yet)"

# TODO: change the capslock to control programatically
echo "* change the capslock to control"
# TODO: change the window focus to follow the mouse programatically
echo "* change the window focus to follow the mouse"
# TODO: change the terminal key shortcuts programatically
echo "* change the terminal key shortcuts"
# gconftool-2 /apps/metacity/global_keybindings/run_command_terminal -s '<Ctrl><Alt>t' -t string
# TODO: add the quicktile shortcuts programatically
echo "* add the quicktile shortcuts"
# TODO: setup dropbox programatically
echo "* setup dropbox"

echo "================================================================"
echo
echo "Finished setting up the new machine, have fun hacking"
