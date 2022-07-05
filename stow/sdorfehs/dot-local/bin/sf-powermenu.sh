#!/usr/bin/env bash
font="$(sdorfehs -c 'set font' | sed -e 's/:size=/ /')"
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
