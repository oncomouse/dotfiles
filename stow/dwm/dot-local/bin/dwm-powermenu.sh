#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
rofifont=$([ "$DOTFILES_TARGET" = "laptop" ] && echo "JetBrainsMono Nerd Font Normal 9" || echo "FiraCode Nerd Font Normal 10")
choice="$(printf "Lock\nSuspend\nLogoff\nRestart\nShutdown" | rofi -dmenu -theme ~/dotfiles/conf/rofi/barmenu.rasi -match fuzzy -auto-select -i -p Powermenu -font "$rofifont")"
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

