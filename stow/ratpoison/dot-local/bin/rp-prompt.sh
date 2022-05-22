#!/usr/bin/env bash

XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CACHE_HOME
mkdir -p "$XDG_CACHE_HOME/ratpoison"

file="$XDG_CACHE_HOME/ratpoison/command-history"

if [ "$1" = "startup" ]; then
	rm -f "$file"
	exit
fi

if [ ! -f "$file" ]; then
	touch "$file"
fi

command="$(tac "$file" | rofi -dmenu -match fuzzy -p ":" -no-fixed-num-lines)"
if [ "$command" != '' ]; then
	echo "$command" >> "$file"
	ratpoison -c "$command"
fi
