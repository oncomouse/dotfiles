#!/usr/bin/env bash
# font=${1:-"FiraCode Nerd Font 12"}
# shellcheck disable=1083
# rofi \
# 	-match fuzzy \
# 	-auto-select \
# 	-show drun \
# 	-show-icons {-drun-display-form \{name\}, -window-format "\\{w\\} \\{c\\} \\{t:25\\}"} \
# 	-location 1 \
# 	-theme-str "window { width: 100%; }" \
# 	-font "$font"
# font="$(echo "$font" | sed -e "s/ \([0-9]\+\)\$/:size=\1/")"
# dmenu -i -n -l 6 -p drun -fn 'JetBrains Mono Nerd Font:size=11'
font="$(ratpoison -c 'set font' | sed -e 's/:size=/ /')"
j4-dmenu-desktop --dmenu="rofi -match fuzzy -auto-select -dmenu -i -location 1 -theme-str 'window { width: 100%; }' -font \"$font\"" --term="st" &
