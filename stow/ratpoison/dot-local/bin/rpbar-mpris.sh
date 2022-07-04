#!/usr/bin/env bash

status="$(dotfiles-media status)"
if [ "${#status}" -gt 100 ]; then
	status="$(echo "$status" | head -c 99)…"
fi
if [ "$status" != "栗" ]; then
	echo -n "["
	echo -n "%{A:dotfiles-media play:}"
	echo -n "%{A2:dotfiles-media prev:}"
	echo -n "%{A3:dotfiles-media next:}"
	echo -n "$status"
	echo -n "]"
	echo -n "%{A}"
	echo -n "%{A}"
	echo -n "%{A}"
fi
echo ""
# vim:ft=sh
