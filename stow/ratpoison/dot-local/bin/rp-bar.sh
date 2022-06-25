#!/usr/bin/env bash
#shellcheck disable=1090

source ~/.cache/wal/colors.sh
pkill lemonaid; pkill lemonbar
# shellcheck disable=2154
lemonaid | lemonbar \
	-a 20 \
	-p -d \
	-f "JetBrains Mono Nerd Font:style=medium:size=10" \
	-B "$color0" \
	-F "$color7" \
	-g x22 | /usr/bin/env bash &

