#!/usr/bin/env bash
scripts_dir=`dirname $0`
cd $scripts_dir/..
repo_dir=$PWD
rm -rf ~/.emacs.d
rm -f ~/.emacs ~/.magit.emacs
ln -sf $repo_dir/emacs.d ~/.emacs.d
ln -sf $repo_dir/docker/ec ~/bin
ln -sf $repo_dir/docker/ed ~/bin
docker_version=$(docker version | sed -n '/Server:/,$P' | grep Version: | cut -d: -f2 | tr -d ' \t\n')
minimum_docker_version=18.0.0
higher_version=$(echo -e "$minimum_docker_version\n$docker_version" | sort -V | tail -n+2)
if [ "x$higher_version" != "x$docker_version" ]; then
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
    sudo apt-get update && sudo apt-get -y install docker-ce
fi
