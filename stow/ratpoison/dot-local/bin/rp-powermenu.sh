#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
font=${1:-"FiraCode Nerd Font 12"}
choice="$(printf "Lock\nSuspend\nLogoff\nRestart\nShutdown" | rofi -dmenu -match fuzzy -auto-select -i -p Powermenu -location 1 -theme-str "window { width: 100%; }" -no-fixed-num-lines -font "$font")"
case $choice in
	Lock)
		xscreensaver-command -lock
		;;
	Suspend)
		xscreensaver-command -lock && xset dpms force off
		;;
	Logoff)
		pkill dwm
		;;
	Restart)
		reboot
		;;
	Shutdown)
		poweroff
		;;
esac
# vim:ft=sh