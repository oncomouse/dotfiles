#!/usr/bin/env bash
if [ "$(ratpoison -c windows | grep -c ^.)" = 1 ]; then
	ratpoison -c "echo No other window"
	exit
fi
font=${1:-"FiraCode Nerd Font 12"}
choice="$(ratpoison -c "windows (%n)%a:%t" | rofi -dmenu -match fuzzy -auto-select -i -p Window -location 1 -theme-str "window { width: 100%; }" -no-fixed-num-lines -font "$font" | sed -e "s/(\([0-9]\+\)).*\$/\1/")"
if [ -n "$choice" ]; then
	ratpoison -c "select $choice" &
fi
