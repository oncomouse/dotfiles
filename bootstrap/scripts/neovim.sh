#!/usr/bin/env bash
# Source Install:
if [ ! -d ~/Projects/neovim ]; then
	git clone https://github.com/neovim/neovim ~/Projects/neovim
	owd="$(pwd)"
	cd ~/Projects/neovim || exit
else
	owd="$(pwd)"
	cd ~/Projects/neovim || exit
	git pull
fi
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install
cd "$owd" || exit
