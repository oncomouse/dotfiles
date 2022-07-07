#!/usr/bin/env bash
font=${1:-"FiraCode Nerd Font 12"}
offset=0
if pgrep sdorfehs > /dev/null; then
	font="$(sdorfehs -c 'set font')"
	font_size="$(echo "$font" | cut -d "=" -f 2)"
	font="${font/:size=/ }"
	padding="$(sdorfehs -c 'set padding' | cut -d " " -f 2)"
	barpadding="$(sdorfehs -c 'set barpadding' | cut -d " " -f 2)"
	offset=$((padding + barpadding * 2 + font_size + 2))
fi
choice=$(printf "契 Play/Pause\n栗 Stop\n玲 Previous\n怜 Next%s" "$(pgrep -x mpd &> /dev/null && printf "\n Search\n Add Album")" | \
	rofi \
	-match fuzzy \
	-auto-select \
	-no-fixed-num-lines \
	-dmenu \
	-i \
	-font "$font" \
	-p "$(dotfiles-media status | sed -e "s/栗/栗 Stopped/")" \
	-location 1 \
	-theme-str "window { width: 100%; y-offset: $offset; }" \
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
esac
