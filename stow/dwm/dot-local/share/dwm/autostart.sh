#!/usr/bin/env bash
source ~/dotfiles/conf/xorg/xinit-common
dex -a
source ~/.cache/wal/colors.sh
xrdb -merge ~/.cache/wal/dwm.Xresources
xrdb -merge ~/.cache/wal/dmenu.Xresources

convert ~/.cache/wal/background.svg ~/.cache/wal/background.png
hsetroot -tile ~/.cache/wal/background.png

dunst&

pkill dwmblocks
dwmblocks&

