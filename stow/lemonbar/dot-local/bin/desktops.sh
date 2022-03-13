#!/usr/bin/env bash

number_of_desktops="$(xprop -root _NET_NUMBER_OF_DESKTOPS | sed -e "s/^\S\+ = //")"
current_desktop="$(xprop -root _NET_CURRENT_DESKTOP | sed -e "s/^\S\+ = //")"
desktop_names="[$(xprop -root _NET_DESKTOP_NAMES | sed -e "s/^\S\+ = //")]"

for (( i=0; i<number_of_desktops; i++ )); do
	desktop_name="$(echo "$desktop_names" | jq .["$i"] | sed -e 's/"//g')"
	if [ $current_desktop = $i ]; then
		echo -n "Current "
	fi
	echo "$desktop_name"
done
