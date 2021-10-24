#!/usr/bin/env bash

source ~/.cache/wal/colors.sh

killall -q mako
while pgrep -x mako >/dev/null; do sleep 1; done
exec mako \
	--font 'FiraCode Nerd Font 10' \
	--default-timeout 5000 \
	--layer overlay \
	--anchor top-right \
	--width 310 \
	--height 100 \
	--border-radius 0 \
	--border-size 2 \
	--max-icon-size 128 \
	--background-color "$color8" \
	--border-size 2 \
	--max-icon-size 128 \
	--text-color "$color7" \
	--border-color "$color8"
