#!/usr/bin/env bash

ocd="$(pwd)"
if [ ! -d ~/.local/share/dwm-config/sdorfehs ]; then
	mkdir -p ~/.local/share/dwm-config
	git clone https://github.com/jcs/sdorfehs ~/.local/share/dwm-config/sdorfehs
fi
cd ~/.local/share/dwm-config/sdorfehs || exit
git pull
if which gcc 2> /dev/null; then
	sudo make install
fi
cd "$ocd" || exit
