#!/bin/sh 
rm -f saja-out
echo "" > saja-out
tmux new-session -d 'tail -f saja-out'
tmux split-window -v -p 10 './saja.byte >> saja-out; rm -f saja-out; tmux kill-window'
tmux -2 attach-session -d 
