#!/usr/bin/env bash

action-add() {
	mpc -f "%file%" search any " " | cut -d "/" -f 1-2 | uniq |
		fzf --reverse -m |
		xargs -d "\n" -I{} mpc search filename {} 2>/dev/null |
		grep "\.\(flac\|mp3\)\$" |
		xargs -d "\n" -I {} mpc add {} 2>/dev/null
}

action-search() {
	mpc play "$(mpc playlist -f "%position%. %artist% - %title%" | fzf --reverse --no-sort | cut -d "." -f 1)" 2>/dev/null >/dev/null
}

action-remove() {
	playlist="$(mpc playlist -f "%position% %artist% - %album%")"
	choice="$(echo "$playlist" | sed -e "s/^[0-9]\\+ //" | uniq | fzf --reverse -m)"
	if [ "$choice" != "" ]; then
		echo "$playlist" | grep "$choice" | tac | cut -d " " -f 1 | xargs -I {} mpc del {}
	fi
}

if [[ $(type -t "action-${1-}") == function ]]; then
	"action-$1" "${@:2}"
else
	action="$(printf "Play/Pause\nStop\nNext\nPrevious\nSearch\nAdd Album\nRemove Album" | fzf --reverse -1 | tr '[:upper:]' '[:lower:]' | cut -d "/" -d " " -f1)"
	case "$action" in
	stop | next)
		mpc "$action"
		;;
	play)
		mpc toggle
		;;
	previous)
		mpc prev
		;;
	*)
		"action-$action"
	esac
fi
