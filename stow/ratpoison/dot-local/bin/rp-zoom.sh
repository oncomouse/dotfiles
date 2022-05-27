#!/usr/bin/env bash

unzoom() {
	ratpoison -c "frestore $(cat "$file")"
	rm "$file"
}

zoom() {
	ratpoison -c fdump > "$file"
	ratpoison -c only
}

XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CACHE_HOME
mkdir -p "$XDG_CACHE_HOME/ratpoison"

file="$XDG_CACHE_HOME/ratpoison/zoom-$(rpws current)"

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
