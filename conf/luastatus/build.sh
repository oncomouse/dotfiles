#!/usr/bin/env bash

owd="$(pwd)"
if [ ! -d ~/.local/share/dotfiles-build/luastatus ]; then
	git clone https://github.com/shdown/luastatus ~/.local/share/dotfiles-build/luastatus
fi
cd ~/.local/share/dotfiles-build/luastatus || exit
git checkout -- %
git pull
git apply ~/dotfiles/conf/luastatus/luastatus-pulse.patch
cmake .
make
sudo make install
cd "$owd" || exit
