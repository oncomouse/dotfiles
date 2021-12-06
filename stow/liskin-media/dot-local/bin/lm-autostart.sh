#!/usr/bin/env bash

DOTFILES_TARGET="$( [ -e ~/.local/share/dotfiles/target ] && /bin/cat ~/.local/share/dotfiles/target)"
if [ "$DOTFILES_TARGET" != "laptop" ]; then
	pkill -f liskin-media
	exec liskin-media mpris-daemon
fi
