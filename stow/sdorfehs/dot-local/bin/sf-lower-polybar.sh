#!/usr/bin/env bash
root=$(xprop -root _NET_SUPPORTING_WM_CHECK | cut -d "#" -f 2)
for x in $(xwininfo -tree -root | grep Polybar | sed -e "s/^\s\+//" | cut -d " " -f 1); do
	xdo above -t "$root" "$x"
done
