#!/usr/bin/env bash
icon=""
if command -v xbacklight > /dev/null; then
	output="$(xbacklight 2> /dev/null | cut -d . -f 1)"
	if [ ${#output} -gt 0 ]; then
		echo -n "%{A:dotfiles-brightness default:}"
		echo -n "["
		echo -n "$icon$output%"
		echo -n "]"
		echo -n "%{A}"
		echo ""
	else
		echo ""
	fi
else
	echo ""
fi
