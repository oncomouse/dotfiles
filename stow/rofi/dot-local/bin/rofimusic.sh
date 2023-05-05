#!/usr/bin/env bash
font="$(rofi-font "$1")"
offset="$(rofi-offset)"
choice=$(printf "󰐊 Play/Pause\n󰓛 Stop\n󰒮 Previous\n󰒭 Next%s" "$(if pgrep -x mpd &> /dev/null || pgrep -x mopidy &> /dev/null; then printf "\n󰩊 Search\n󰜄 Add Album\n Remove Album"; fi)" | \
	rofi \
	-match fuzzy \
	-auto-select \
	-no-fixed-num-lines \
	-dmenu \
	-i \
	-font "$font" \
	-p "$(dotfiles-media status | sed -e "s/󰓛/󰓛 Stopped/")" \
	-location 1 \
	-theme-str "window { y-offset: $offset; }" \
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
		mpd_rofi.sh search "$font"
		;;
	*Add*)
		mpd_rofi.sh add "$font"
		;;
	*Remove*)
		mpd_rofi.sh remove "$font"
		;;
esac
