#!/usr/bin/env bash

case "$1" in
	"up")
		sudo light -A 5
		;;
	"down")
		sudo light -U 5
		;;
	"default")
		sudo light -S 50
		;;
	*)
		;;
esac
kill -38 "$(pidof waybar)" 2> /dev/null
