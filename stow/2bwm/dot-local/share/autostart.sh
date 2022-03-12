#!/usr/bin/env bash
source ~/dotfiles/conf/xorg/xinit-common
dex -a
picom --experimental-backends -b --config ~/dotfiles/conf/picom/common.conf

convert ~/.cache/wal/background.svg ~/.cache/wal/background.png
hsetroot -tile ~/.cache/wal/background.png
