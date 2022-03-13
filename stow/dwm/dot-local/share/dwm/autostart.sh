#!/usr/bin/env bash
source ~/dotfiles/conf/xorg/xinit-common
dex -a

xrdb -merge ~/.cache/wal/dwm.Xresources
xrdb -merge ~/.cache/wal/dmenu.Xresources

source ~/dotfiles/conf/xorg/xinit.d/no-xfce4
source ~/dotfiles/conf/xorg/xinit.d/dunst

convert ~/.cache/wal/background.svg ~/.cache/wal/background.png
hsetroot -tile ~/.cache/wal/background.png

pkill dwmblocks
dwmblocks&
