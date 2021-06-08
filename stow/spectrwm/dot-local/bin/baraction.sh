#!/usr/bin/env bash

# fifo="/tmp/panel_fifo"
# [ -e "$fifo" ] && rm "$fifo"
# mkfifo "$fifo"
conky -c ~/dotfiles/conf/conky/spectrwm.lua
