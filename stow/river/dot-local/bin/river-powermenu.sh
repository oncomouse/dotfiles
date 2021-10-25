#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
if [ "$DOTFILES_TARGET" == "laptop" ]; then
	rofifont="Hack Nerd Font 9"
else
	rofifont="FiraCode Nerd Font 10"
fi
source ~/.cache/wal/colors.sh
choice="$(printf "Lock\nSuspend\nLogoff\nRestart\nShutdown" | rofi -theme ~/dotfiles/conf/rofi/barmenu.rasi -match fuzzy -auto-select -font "$rofifont" -dmenu -p Powermenu -i)"
case $choice in
	Lock)
		swaylock -c "$color0"
		;;
	Suspend)
		swaylock -c "$color0"
		;;
	Logoff)
		riverctl exit
		;;
	Restart)
		reboot
		;;
	Shutdown)
		poweroff
		;;
esac

