#!/usr/bin/env bash
# shellcheck disable=SC2119,SC2120

font=${2:-"FiraCode Nerd Font 12"}

rofi_bar() {
	rofi -match fuzzy -dmenu -i -font "$font" -location 1 -theme-str "window { width: 100%; }" "$@"
}

action-search() {
	match="$(mpc playlist | rofi_bar)"
	if [[ "$match" != "" ]]; then
		match_pos="$(mpc playlist | grep -n "$match" | cut -d : -f 1 | head -n 1)"
		mpc play "$match_pos" >/dev/null
	fi
}

action-add() {
	# albums="$(mpc ls | while IFS= read -r artist; do
	# 	mpc ls "$artist" 2> /dev/null | while IFS= read -r album; do
	# 		printf "%s\n" "$album"
	# 	done
	# done)"
	# match="$(printf "%s" "$albums" | rofi_bar)"
	# if [[ "$match" != "" ]]; then
	# 	 mpc ls "$match" 2> /dev/null | while IFS= read -r song; do
	# 	 mpc add "$song"
	#  done
	# fi

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
