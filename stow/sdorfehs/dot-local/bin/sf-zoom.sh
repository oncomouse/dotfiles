#!/usr/bin/env bash

unzoom() {
	sdorfehs -c "frestore $(cat "$file")"
	rm "$file"
}

zoom() {
	sdorfehs -c fdump > "$file"
	sdorfehs -c only
}

XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CACHE_HOME
mkdir -p "$XDG_CACHE_HOME/sdorfehs"

file="$XDG_CACHE_HOME/sdorfehs/zoom-$(sdorfehs -c vscreens | grep -f "*" | cut -d "*" -f 1)"

if [ "$1" = "check" ]; then
	if [ -e "$file" ]; then
		unzoom
	fi
	exit
fi

if [ -e "$file" ]; then
	unzoom
else
	zoom
fi
