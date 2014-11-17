#/usr/bin/env bash

for i in "$@"
do
    tmux split-window "ssh $i"
    tmux select-layout tiled
done
tmux set-window-option synchronize-panes on
tmux select-layout tiled
