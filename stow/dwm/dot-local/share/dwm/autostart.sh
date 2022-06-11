#!/usr/bin/env bash
#shellcheck disable=1090
source ~/dotfiles/conf/xorg/xinit-common
dex -a
picom --experimental-backends -b --config ~/dotfiles/conf/picom/dwm.conf

source ~/dotfiles/conf/xorg/xinit.d/no-xfce4
source ~/dotfiles/conf/xorg/xinit.d/dunst
source ~/dotfiles/conf/xorg/xinit.d/no-powerbutton
source ~/dotfiles/conf/xorg/xinit.d/dotfiles-media
source ~/dotfiles/conf/xorg/xinit.d/tile-background

DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
[ "$DOTFILES_TARGET" = "laptop" ] && cbatticon&
