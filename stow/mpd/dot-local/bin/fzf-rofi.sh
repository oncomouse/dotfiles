#!/usr/bin/env bash

action-add() {
	mpc ls | \
		xargs -d "\n" -I{}  mpc ls {} 2> /dev/null | \
		fzf -m | \
		xargs -d "\n" -I{} mpc ls {} 2> /dev/null | \
		xargs -d "\n" -I {} mpc add {} 2> /dev/null
}

action-search() {
	mpc play "$(mpc playlist -f "%position%. %artist% - %title%" | fzf --no-sort | cut -d "." -f 1)" 2> /dev/null > /dev/null
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action-search
fi