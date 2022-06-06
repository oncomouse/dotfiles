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
	choice="$(mpc ls | while IFS= read -r artist; do mpc ls "$artist" 2> /dev/null | while IFS= read -r album; do printf "%s\n" "$album"; done; done | rofi_bar)"
	if [[ -n "$choice" ]]; then
			 mpc ls "$choice" 2> /dev/null | while IFS= read -r song; do
			 mpc add "$song"
		 done
	fi
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi
