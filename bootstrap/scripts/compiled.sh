#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st dwm dwmblocks dmenu sdorfehs
cd "$ocd" || exit
