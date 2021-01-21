#!/bin/bash

# Modified from: https://github.com/serhanekmekci/dotfiles/blob/413593862782ac105e48de644aed842eafa14797/.local/bin/cmdvim

for socket in $(nvr --nostart --serverlist); do
  nvr --nostart --servername $socket -cc "$1"
done
