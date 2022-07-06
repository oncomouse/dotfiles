#!/usr/bin/env bash
if [ "$(sdorfehs -c windows | grep -c ^.)" = 1 ]; then
	sdorfehs -c "echo No other window"
	exit
fi
font="$(sdorfehs -c 'set font' | sed -e 's/:size=/ /')"
choice="$(sdorfehs -c "windows (%n)%a:%t" | rofi -dmenu -match fuzzy -auto-select -i -p Window -location 1 -theme-str "window { width: 100%; y-offset: 20px; }" -no-fixed-num-lines -font "$font" | sed -e "s/(\([0-9]\+\)).*\$/\1/")"
if [ -n "$choice" ]; then
	sdorfehs -c "select $choice" &
fi
