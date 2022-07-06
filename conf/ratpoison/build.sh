#!/usr/bin/env bash

ocd="$(pwd)"
mkdir -p ~/Projects
git clone https://github.com/oncomouse/ratpoison ~/Projects/ratpoison
cd ~/Projects/ratpoison || exit
if which autoconf 2> /dev/null; then
	./autogen.sh || exit
	./configure || exit
	make
	sudo make install
fi
cd "$ocd" || exit
