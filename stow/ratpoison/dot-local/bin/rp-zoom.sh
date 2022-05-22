#!/usr/bin/env bash

XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CACHE_HOME
mkdir -p "$XDG_CACHE_HOME/ratpoison"

file="$XDG_CACHE_HOME/ratpoison/zoom-$(rpws current)"

if [ -e "$file" ]; then
	ratpoison -c "frestore $(cat "$file")"
	rm "$file"
else
	ratpoison -c fdump > "$file"
	ratpoison -c only
fi
