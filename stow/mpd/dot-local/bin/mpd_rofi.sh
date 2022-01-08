#!/bin/bash
# shellcheck disable=SC2119,SC2120

rofi_bar() {
	rofi -match fuzzy -auto-select -dmenu -i -location 1 -theme-str "window { width: 100%; }" "$@"
}

action-search() {
	match="$(mpc playlist | rofi_bar)"
	if [[ "$match" != "" ]]; then
		match_pos="$(mpc playlist | grep -n "$match" | cut -d : -f 1)"
		mpc play "$match_pos" > /dev/null
	fi
}

action-add() {
	match="$(fd --type d --exact-depth 2 . /mnt/Media/Music/ | cut -d / -f 5-6 | rofi_bar)"
	if [[ "$match" != "" ]]; then
		fd -e mp3 -e flac . /mnt/Media/Music/"$match" --print0 | while read -rd $'\0' file; do
			mpc add "$(echo "$file" | sed -e "s/\/mnt\/Media\/Music\///")"
		done
	fi
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi
