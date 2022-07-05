#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st dwm dwmblocks dmenu lemonaid sdorfehs
cd "$ocd" || exit
