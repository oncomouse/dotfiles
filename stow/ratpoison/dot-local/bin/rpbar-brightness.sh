#!/usr/bin/env bash
icon=""
if command -v backlight; then
	output="$(xbacklight 2> /dev/null | cut -d . -f 1)"
	if [ ${#output} -gt 0 ]; then
		echo -n "["
		echo -n "%{A1:dotfiles-brightness default:}"
		echo -n "%{A4:dotfiles-brightness up:}"
		echo -n "%{A5:dotfiles-brightness down:}"
		echo -n "$icon $output%"
		echo -n "%{A}"
		echo -n "%{A}"
		echo -n "%{A}"
		echo "]"
	fi
else
	echo ""
fi
