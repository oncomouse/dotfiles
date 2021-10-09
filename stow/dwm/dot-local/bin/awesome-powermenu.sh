#!/usr/bin/env bash
choice="$(printf "Lock\nSuspend\nLogoff\nRestart\nShutdown" | rofi -dmenu -theme ~/dotfiles/conf/rofi/barmenu.rasi -n -F -i -p Powermenu "$@")"
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

