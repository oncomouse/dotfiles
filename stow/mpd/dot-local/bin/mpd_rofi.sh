#!/bin/bash

rofi_cmd="fd --type d --exact-depth 2 . /mnt/Media/Music/ | cut -d / -f 5-6"

action-search() {
	match="$(mpc playlist | $rofi_cmd)"
	if [[ "$match" != "" ]]; then
		match_pos="$(mpc playlist | grep -n "$match" | cut -d : -f 1)"
		mpc play "$match_pos" > /dev/null
	fi
}

action-add() {
	match="$(fd --type d --exact-depth 2 . /mnt/Media/Music/ | cut -d / -f 5-6 | $rofi_cmd)"
	if [[ "$match" != "" ]]; then
		for x in $(/mnt/Media/$(match)/*.{flac,mp3} 2> /dev/null); do
			mpc add "$x"
		done
	fi
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi
