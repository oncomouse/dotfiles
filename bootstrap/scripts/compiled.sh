#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st dwm dwmblocks dmenu sdorfehs luastatus
cd "$ocd" || exit
