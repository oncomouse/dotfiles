#!/usr/bin/env bash
if [ "$(ratpoison -c windows | grep -c ^.)" = 1 ]; then
	ratpoison -c "echo No other window"
	exit
fi
ratpoison -c "select $(ratpoison -c "windows (%n)%a:%t" | rofi -dmenu -auto-select | sed -e "s/(\([0-9]\+\)).*\$/\1/")" &
