#!/usr/bin/env bash

resolution="$(wlr-randr | rg "[0-9]+x[0-9]+ px" | cut -d , -f 1 | sed -e "s/^\s\+//" -e "s/ px//")"
width="$(echo "$resolution" | cut -d x -f 1)"
# height="$(echo "$resolution" | cut -d x -f 2)"

ratio=$(printf "%.0f" "$(echo "($width - $width*.85) / 2" | bc)")

# Monocle Layout
riverctl send-layout-cmd stacktile "all_primary true"
riverctl send-layout-cmd stacktile "primary_sublayout full"
riverctl send-layout-cmd stacktile "primary_position left"
riverctl send-layout-cmd stacktile "primary_count 2"
riverctl send-layout-cmd stacktile "primary_ratio 0.5"
riverctl send-layout-cmd stacktile "outer_padding $ratio"
riverctl send-layout-cmd stacktile "inner_padding 3"
riverctl send-layout-cmd stacktile "secondary_sublayout rows"
riverctl send-layout-cmd stacktile "secondary_count 0"
riverctl send-layout-cmd stacktile "secondary_ratio 0.5"
riverctl send-layout-cmd stacktile "remainder_sublayout stack"

