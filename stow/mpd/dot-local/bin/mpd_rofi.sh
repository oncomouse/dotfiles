#!/usr/bin/env bash

font="$(rofi-font "$1")"
offset="$(rofi-offset)"

# shellcheck disable=SC2119,SC2120
rofi_bar() {
	rofi -match fuzzy -dmenu -i -font "$font" -location 1 -theme-str "window { width: 100%; y-offset: $offset; }" "$@"
}

action-search() {
	match="$(mpc playlist | rofi_bar)"
	if [[ "$match" != "" ]]; then
		match_pos="$(mpc playlist | grep -n "$match" | cut -d : -f 1 | head -n 1)"
		mpc play "$match_pos" >/dev/null
	fi
}

action-add() {
	# Get a list of artists \
	#	Get a list of albums \
	#	Select albums \
	#	Get a list of songs in the selected albums \
	#	Add the songs to MPD
	mpc ls | \
		xargs -d "\n" -I{}  mpc ls {} 2> /dev/null | \
		rofi_bar -multi-select | \
		xargs -d "\n" -I{} mpc ls {} 2> /dev/null | \
		xargs -d "\n" -I {} mpc add {} 2> /dev/null
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi
