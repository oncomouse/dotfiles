#!/usr/bin/env bash
DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
if [ "$DOTFILES_TARGET" == "laptop" ]; then
	font="Hack Nerd Font 9"
else
	font="FiraCode Nerd Font 10"
fi

source ~/.cache/wal/colors.sh

killall -q mako
while pgrep -x mako >/dev/null; do sleep 1; done
exec mako \
	--font "$font" \
	--default-timeout 5000 \
	--layer overlay \
	--anchor top-right \
	--width 310 \
	--height 100 \
	--border-radius 0 \
	--border-size 2 \
	--max-icon-size 128 \
	--background-color "$color8" \
	--border-size 2 \
	--max-icon-size 128 \
	--text-color "$color7" \
	--border-color "$color8"
