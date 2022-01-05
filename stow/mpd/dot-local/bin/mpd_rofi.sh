#!/bin/bash

match="$(mpc playlist | rofi -match fuzzy -auto-select -dmenu -i -location 1 -theme-str "window { width: 100%; }")"
if [[ "$match" != "" ]]; then
	match_pos="$(mpc playlist | grep -n "$match" | cut -d : -f 1)"
	mpc play "$match_pos" > /dev/null
fi
