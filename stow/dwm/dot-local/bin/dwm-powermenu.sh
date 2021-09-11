#!/usr/bin/env bash
dwm_colors() {
	xrdb -query | rg dwm | sed -e 's/^dwm\.//' -e 's/:\s\+/=/'
}
eval "$(dwm_colors)"
choice="$(printf "Lock\nSuspend\nLogoff\nRestart\nShutdown" | dmenu -n -F -i -p Powermenu -fn "Hack-Regular:size=9" -nb "$normbgcolor" -nf "$normfgcolor" -sb "$tagsselbgcolor" -sf "$tagsselfgcolor")"
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
