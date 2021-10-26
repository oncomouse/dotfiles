#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
if [ "$DOTFILES_TARGET" == "laptop" ]; then
	config="-c $HOME/.config/waybar/config-laptop"
	style="-s $HOME/.config/waybar/style-laptop.css"
else
	config=""
	style=""
fi

killall -q waybar
while pgrep -x waybar >/dev/null; do sleep 1; done
exec waybar "$config" "$style"
