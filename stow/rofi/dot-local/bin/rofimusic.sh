#!/usr/bin/env bash

font=${1:-"FiraCode Nerd Font 12"}
choice=$(printf "契 Play/Pause\n栗 Stop\n玲 Previous\n怜 Next" | \
	rofi \
	-match fuzzy \
	-auto-select \
	-dmenu \
	-i \
	-font "$font" \
	-p "$(liskin-media status)" \
	"-location" \
	"1" \
	"-theme-str" \
	"window { width: 100%; }" \
)
case "$choice" in
	*Play*)
		liskin-media play
		;;
	*Stop*)
		liskin-media stop
		;;
	*Previous*)
		liskin-media prev
		;;
	*Next*)
		liskin-media next
		;;
esac
