#!/usr/bin/env bash

function add_repo {
    if [ $# -ne 1 ]; then
        echo "You should pass exactly one arg to this function, the repo ppa:... to be added"
        echo "This is probably a bug in the script abandon ship"
        exit 1
    fi
    search_for=${1/*://}/ubuntu
    if ! grep -R $search_for /etc/apt/sources.list.d/ > /dev/null 2>&1; then
        sudo add-apt-repository $1
        sudo apt-get update
    fi
}

config_repo=$(readlink -f $(dirname $0))
repos_dir="$HOME/codez"

if [ ! -d $repos_dir ]; then
    repos_dir="$PWD/.."
fi

add_repo ppa:cassou/emacs
# the following ppa doesn't seem to work, this is the ag (faster ack)

# add_repo ppa:ggreer/ag
add_repo ppa:git-core/ppa
add_repo ppa:webupd8team/java
add_repo ppa:sun-java-community-team/sun-java6

[ -a ~/.Xmodmap ] || ln -s $config_repo/`hostname`.xmodmap ~/.Xmodmap
[ -d ~/.emacs.d ] || ln -s $config_repo ~/.emacs.d
[ -f ~/.emacs   ] || ln -s $config_repo/.emacs ~/.emacs

if ! grep 'DO NOT REPLACE' ~/.bashrc > /dev/null 2>&1; then
    ln -s $config_repo/.bashrc ~/.bashrc
    . ~/.bashrc
fi

if [ ! -f ~/.ackrc ]; then
    ln -s $config_repo/.ackrc ~/.ackrc
fi

# add google chrome ppa
if ! grep -R linux/chrome /etc/apt/sources.list.d/ > /dev/null 2>&1; then
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
    sudo apt-get update
fi

sudo apt-get --ignore-missing install \
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
    ghc-doc \
    skype \
    postgresql \
    postgresql-client \
    postgresql-server-dev-all \
    postgresql-contrib \
    google-chrome-stable \
    wireshark \
    irssi \
    libyaml-dev \
    nmap \
    openjdk-7-source \
    openjdk-6-source \
    gnome-disk-utility \
    imagemagick \
    git-svn \
    tofrodos \
    libcurl4-gnutls-dev \
    apache2 \
    phantomjs \
    coffeescript \
    oracle-java7-installer \
    ia32-sun-java6-bin \
    cifs-utils \
    valgrind \
    virtualbox

# download and setup repo (the android repo management script)
[ -d ~/bin ] || mkdir ~/bin
curl https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
chmod a+x ~/bin/repo
ln -s $PWD/generate_tags.sh ~/bin/
ln -s $PWD/cleanup_shit.sh ~/bin/

# download and install synergy
if ! dpkg -l | grep synergy > /dev/null 2>&1; then
    pushd /tmp
    wget http://synergy.googlecode.com/files/synergy-1.4.10-Linux-$(uname -m).deb
    sudo dpkg -i synergy-1.4.10-Linux-$(uname -m).deb
    popd
fi

if sudo dmidecode --type 1 | grep -i lenovo 2>&1 > /dev/null; then
    add_repo ppa:fingerprint/fingerprint-gui
    add_repo ppa:bumblebee/stable
    add_repo ppa:ubuntu-x-swat/x-updates
    sudo apt-get install bumblebee \
        bumblebee-nvidia \
        bbswitch-dkms \
        linux-headers-generic \
        libbsapi \
        policykit-1-fingerprint-gui \
        fingerprint-gui
fi

# this might fail on old distros
sudo apt-get install openjdk-7-jdk openjdk-6-jdk

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

# setup the ensime git repo
pushd $repos_dir
[ -d ensime ] || git clone git@github.com:jvshahid/ensime.git
[ -d coffee-mode ] || git clone git@github.com:jvshahid/coffee-mode.git
pushd coffee-mode
target=~/.emacs.d/libs/coffee-mode
if [ ! -f $target/coffee-mode.el ]; then
    mkdir -p $target
    ln -s $PWD/coffee-mode.el $target/coffee-mode.el
fi
popd
[ -d forml-mode ] || git clone git@github.com:jvshahid/forml-mode.git
pushd forml-mode
target=~/.emacs.d/libs/forml-mode
if [ ! -f $target/forml-mode.el ]; then
    mkdir -p $target
    ln -s $PWD/forml-mode.el $target/forml-mode.el
fi
popd
pushd ensime
[ -f ./sbt ] || curl 'https://raw.github.com/paulp/sbt-extras/master/sbt' -o ./sbt
chmod a+x ./sbt
[ -d target/dist ] || ./sbt stage
dist_dir=$(readlink -f $(ls -d dist*))
ln -s $dist_dir $HOME/.emacs.d/libs/ensime_head
popd
popd


# setup the git aliases
[ -a $HOME/.gitconfig ] || ln -s $config_repo/.gitconfig $HOME/.gitconfig

# Setup rvm
if [ ! -d $HOME/.rvm ]; then
    curl -L https://get.rvm.io | bash -s stable --ruby
    source ~/.rvm/scripts/rvm
fi

# get the requirements for ruby 1.9.3
requirements=$(rvm requirements | grep '\bruby:' | sed 's/.*apt-get install\(.*\)/\1/')
sudo apt-get install $requirements
( rvm list | grep 1.9.3 > /dev/null 2>&1 ) || ( rvm install 1.9.3 && rvm use --default 1.9.3 )
( rvm list | grep jruby-1.6.7 > /dev/null 2>&1 ) || rvm install jruby-1.6.7
( rvm list | grep jruby-1.7.0 > /dev/null 2>&1 ) || rvm install jruby-1.7.0
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

# Setup wireshark to allow non root users to capture packets (answer yes)
sudo dpkg-reconfigure wireshark-common
sudo usermod -a -G wireshark $USER
echo "==========================================================================================="
echo "IMPORTANT: You will have to logout and log back in for wireshark permissions to take effect"
echo "==========================================================================================="

echo
echo
echo "===================================================================================="
echo "don't forgot to make the following changes by hand (they aren't automated yet)"

# TODO: change the capslock to control programatically
echo "* change the capslock to control or disable it"
# TODO: change the window focus to follow the mouse programatically
echo "* change the window focus to follow the mouse"
# TODO: change the terminal key shortcuts programatically
echo "* change the terminal key shortcuts"
# gconftool-2 /apps/metacity/global_keybinndings/run_command_terminal -s '<Ctrl><Alt>t' -t string
# TODO: add the quicktile shortcuts programatically and change workspace switching shortcuts
echo "* add the quicktile shortcuts and change the workspace switching shortcuts"
# TODO: setup dropbox programatically
echo "* setup dropbox"

echo "===================================================================================="
echo
echo "Finished setting up the new machine, have fun hacking"


# setup the firewall

# Block access to 1.2.3.4 (vodafone image resolution modification proxy)
sudo iptables -A OUTPUT -p tcp -d 1.2.3.4 --dport 80 -j REJECT --reject-with tcp-reset

# finally clean up shit that I don't need
sudo apt-get autoremove
