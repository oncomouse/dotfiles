#!/usr/bin/env bash
root=$(xprop -root _NET_SUPPORTING_WM_CHECK | cut -d "#" -f 2)
bar=$(xwininfo -tree -root | grep '"polybar" "Polybar"' | sed -e "s/^\s\+//" | cut -d " " -f 1)
xdo above -t "$root" "$bar"
for x in $(xwininfo -tree -root | grep '"tray" "Polybar"' | sed -e "s/^\s\+//" | cut -d " " -f 1); do
	xdo above -t "$bar" "$x"
done
