#!/usr/bin/env bash
source ~/dotfiles/conf/xorg/xinit-common
dex -a
picom --experimental-backends -b --config ~/dotfiles/conf/picom/dwm.conf

source ~/dotfiles/conf/xorg/xinit.d/no-xfce4
source ~/dotfiles/conf/xorg/xinit.d/dunst

convert ~/.cache/wal/background.svg ~/.cache/wal/background.png
hsetroot -tile ~/.cache/wal/background.png

pkill -f liskin-media
liskin-media mpris-daemon&

pkill dwmblocks
dwmblocks&
