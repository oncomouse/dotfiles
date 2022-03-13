#!/usr/bin/env bash

number_of_desktops="$(xprop -root _NET_NUMBER_OF_DESKTOPS | sed -e "s/^\S\+ = //")"
current_desktop="$(xprop -root _NET_CURRENT_DESKTOP | sed -e "s/^\S\+ = //")"
desktop_names="[$(xprop -root _NET_DESKTOP_NAMES | sed -e "s/^\S\+ = //")]"
desktop_windows="$(wmctrl -l | cut -d " " -f 3)"

for (( i=0; i<number_of_desktops; i++ )); do
	current=0
	if [ "$current_desktop" = "$i" ]; then
		current=1
	fi
	occupied=0
	if [ "$(echo "$desktop_windows" | grep -c "$i")" != "0" ]; then
		occupied=1
	fi
	if [ "$occupied" == 1 ] || [ "$current" == 1 ]; then
		desktop_name="$(echo "$desktop_names" | jq .["$i"] | sed -e 's/"//g')"
		echo "$desktop_name"
	fi
done
