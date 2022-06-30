#!/usr/bin/env bash
ocd="$(pwd)"
cd ~/dotfiles || exit
make st dwm dwmblocks dmenu lemonaid tabbed ratpoison
cd "$ocd" || exit
