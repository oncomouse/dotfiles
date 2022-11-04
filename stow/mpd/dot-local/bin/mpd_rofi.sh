#!/usr/bin/env bash

font="$(rofi-font "$1")"
offset="$(rofi-offset)"

# shellcheck disable=SC2119,SC2120
rofi_bar() {
	rofi -match fuzzy -dmenu -i -font "$font" -location 1 -theme-str "window { y-offset: $offset; }" "$@"
}

action-search() {
mpc play "$(mpc playlist -f "%position%. %artist% - %title% (%album%)" | rofi_bar | cut -d "." -f 1)" 2> /dev/null > /dev/null
}

action-add() {
	# Get a list of artists \
	#	Get a list of albums \
	#	Select albums \
	#	Get a list of songs in the selected albums \
	#	Filter out playlists and cue files \
	#	Add the songs to MPD
	mpc -f "%file%" search any " " | cut -d "/" -f 1-2 | uniq | \
		rofi_bar -multi-select | \
		xargs -d "\n" -I{} mpc ls {} 2> /dev/null | \
		grep "\.\(flac\|mp3\)\$" | \
		uniq | \
		xargs -d "\n" -I {} mpc add {} 2> /dev/null
}

action-remove() {
	playlist="$(mpc playlist -f "%position% %artist% - %album%")"
	choice="$(echo "$playlist" | sed -e "s/^[0-9]\\+ //" | uniq | rofi_bar -multi-select)"
	if [ "$choice" != "" ]; then
		echo "$playlist" | grep "$choice" | tac | cut -d " " -f 1 | xargs -I {} mpc del {}
	fi
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi
