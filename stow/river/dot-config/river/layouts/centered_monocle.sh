#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
if [ "$DOTFILES_TARGET" == "laptop" ]; then
	cm_scale=0.95
else
	cm_scale=0.85
fi
resolution="$(wlr-randr | rg "current" | rg "[0-9]+x[0-9]+ px" | cut -d , -f 1 | sed -e "s/^\s\+//" -e "s/ px//")"
width="$(echo "$resolution" | cut -d x -f 1)"
# height="$(echo "$resolution" | cut -d x -f 2)"

ratio=$(printf "%.0f" "$(echo "($width - $width*$cm_scale) / 2" | bc)")
layout="(f)"
source "$HOME/.config/river/utils/kile_layout.sh"
kile_layout "$layout"
riverctl send-layout-cmd kile "outer_padding $ratio"
