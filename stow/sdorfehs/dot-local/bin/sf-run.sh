#!/usr/bin/env bash
font="$(sdorfehs -c 'set font' | sed -e 's/:size=/ /')"
# shellcheck disable=1083
rofi \
	-match fuzzy \
	-auto-select \
	-show drun \
	-show-icons {-drun-display-form \{name\}, -window-format "\\{w\\} \\{c\\} \\{t:25\\}"} \
	-font "$font"
# font="$(sdorfehs -c 'set font' | sed -e 's/:size=/ /')"
# j4-dmenu-desktop --dmenu="rofi -match fuzzy -auto-select -dmenu -i -location 1 -theme-str 'window { width: 100%; }' -font \"$font\"" --term="st" &
