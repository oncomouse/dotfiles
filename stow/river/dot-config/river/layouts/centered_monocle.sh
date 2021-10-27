#!/usr/bin/env bash

# resolution="$(wlr-randr | rg "current" | rg "[0-9]+x[0-9]+ px" | cut -d , -f 1 | sed -e "s/^\s\+//" -e "s/ px//")"
# width="$(echo "$resolution" | cut -d x -f 1)"
# height="$(echo "$resolution" | cut -d x -f 2)"

# ratio=$(printf "%.0f" "$(echo "($width - $width*.85) / 2" | bc)")
layout="(f)"
source "$HOME/.config/river/utils/kile_layout.sh"
kile_layout "$layout"
# riverctl send-layout-cmd kile "xoffset $ratio"
