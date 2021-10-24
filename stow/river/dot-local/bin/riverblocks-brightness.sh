#!/usr/bin/env bash
case $BUTTON in
	1) dwm-brightness.sh default ;;
	4) dwm-brightness.sh down ;;
	5) dwm-brightness.sh up ;;
esac
icon=""
output="$(xbacklight 2> /dev/null | cut -d . -f 1)"
if [ ${#output} -gt 0 ]; then
	echo "$icon $output%"
fi
