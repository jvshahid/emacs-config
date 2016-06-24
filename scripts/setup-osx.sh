#!/usr/bin/env bash
scripts_dir=`dirname $0`
repo_dir=$scripts_dir/..
rm -rf ~/.emacs.d
ln -s $repo_dir/emacs.d ~/.emacs.d
ln -s $repo_dir/cookbooks/development/files/default/.emacs ~/
ln -s $repo_dir/cookbooks/development/files/default/.magit.emacs ~/
