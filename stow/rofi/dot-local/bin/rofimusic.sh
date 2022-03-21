#!/usr/bin/env bash

font=${1:-"FiraCode Nerd Font 12"}
choice=$(printf "契 Play/Pause\n栗 Stop\n玲 Previous\n怜 Next\n Search\n Add Album" | \
	rofi \
	-match fuzzy \
	-auto-select \
	-dmenu \
	-i \
	-font "$font" \
	-p "$(dotfiles-media status)" \
	"-location" \
	"1" \
	"-theme-str" \
	"window { width: 100%; }" \
)
case "$choice" in
	*Play*)
		dotfiles-media play
		;;
	*Stop*)
		dotfiles-media stop
		;;
	*Previous*)
		dotfiles-media prev
		;;
	*Next*)
		dotfiles-media next
		;;
	*Search*)
		mpd_rofi.sh search
		;;
	*Add*)
		mpd_rofi.sh add
		;;
esac
