#!/usr/bin/env bash

if ! which git 2>/dev/null; then
    echo "Cannot find git. I will try to install it"
    sudo apt-get install git-core
fi

scripts_dir=`dirname $0`
cd $scripts_dir/..

repo_dir=$(git rev-parse --show-toplevel 2>/dev/null)

if [ "x$repo_dir" != "x$PWD" ]; then
    git clone https://github.com/jvshahid/emacs-config $HOME/emacs-config
    repo_dir=$HOME/emacs-config
fi

ln -sf $repo_dir/emacs.d ~/.emacs.d
ln -sf $repo_dir/dotfiles/ec ~/bin
ln -sf $repo_dir/dotfiles/ed ~/bin

function docker_version() {
  docker version | sed -n '/Server:/,$P' | grep Version: | cut -d: -f2 | tr -d ' \t\n'
}

# minimum tested docker version
min_docker_version=18.0.0

highest_version=$(echo -e "$min_docker_version\n$(docker_version)" | sort -V | tail -n+2)
if [ "x$highest_version" != "x$(docker_version)" ]; then
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
    sudo apt-get update && sudo apt-get -y install docker-ce
fi
