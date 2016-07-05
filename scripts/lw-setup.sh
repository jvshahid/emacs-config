#!/usr/bin/env bash
scripts_dir=`dirname $0`
cd $scripts_dir/..
repo_dir=$PWD
rm -rf ~/.emacs.d
rm -f ~/.emacs ~/.magit.emacs
ln -s $repo_dir/emacs.d ~/.emacs.d
ln -s $repo_dir/cookbooks/development/files/default/.emacs ~/
ln -s $repo_dir/cookbooks/development/files/default/.magit.emacs ~/
