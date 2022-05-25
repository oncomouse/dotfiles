#!/usr/bin/env bash
source ~/.cache/wal/colors.sh
status="$(pamixer --get-volume-human)"
echo -n "["
echo -n "%{A:dotfiles-volume mute:}"
echo -n "%{A4:dotfiles-volume up:}"
echo -n "%{A5:dotfiles-volume down:}"
if [ "$status" == "muted" ]; then
	echo -n "婢"
else
	echo -n "墳 $status"
fi
echo -n "]"
echo -n "%{A}"
echo -n "%{A}"
echo -n "%{A}"
echo ""
# vim:ft=sh
