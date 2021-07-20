#!/usr/bin/env bash
# Get plugins:
if (( $(git submodule status -- ~/dotfiles | wc -l) == 0 )); then
	git submodule init
	git submodule update
else
	echo "Updaing plugins…"
	git submodule update --remote
fi

# Source Install:
# if [ ! -d ~/Projects/neovim ]; then
# 	git clone https://github.com/neovim/neovim ~/Projects/neovim
# 	owd="$(pwd)"
# 	cd ~/Projects/neovim || exit
# else
# 	owd="$(pwd)"
# 	cd ~/Projects/neovim || exit
# 	git pull
# fi
# make CMAKE_BUILD_TYPE=RelWithDebInfo
# sudo make install
# cd "$owd" || exit
