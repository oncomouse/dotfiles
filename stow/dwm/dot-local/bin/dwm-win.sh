#!/usr/bin/env bash
font=${1:-"FiraCode Nerd Font 12"}
# shellcheck disable=1083
rofi \
	-theme themes/bar-menu.rasi \
	-match fuzzy \
	-auto-select \
	-font "$font" \
	-show window \
	-show-icons \
	-window-format "{w} {c} {t:25}"
