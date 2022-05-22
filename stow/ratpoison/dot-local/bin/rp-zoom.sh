#!/usr/bin/env bash

XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/cache}
export XDG_CACHE_HOME
mkdir -p "$XDG_CACHE_HOME/ratpoison"

if [ -e "$XDG_CACHE_HOME/ratpoison/zoom" ]; then
	ratpoison -c "frestore $(cat "$XDG_CACHE_HOME/ratpoison/zoom")"
	rm "$XDG_CACHE_HOME/ratpoison/zoom"
else
	ratpoison -c fdump > "$XDG_CACHE_HOME/ratpoison/zoom"
	ratpoison -c only
fi
