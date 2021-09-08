#!/usr/bin/env bash
case $BUTTON in
	1) berry-brightness.sh default ;;
	4) berry-brightness.sh down ;;
	5) berry-brightness.sh up ;;
esac
icon=""
output="$(xbacklight 2> /dev/null | cut -d . -f 1)"
if [ ${#output} -gt 0 ]; then
	echo "$icon $output%"
fi
