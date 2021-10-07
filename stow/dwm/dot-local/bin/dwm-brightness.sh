#!/usr/bin/env bash

case "$1" in
	"up")
		xbacklight -inc 5
		;;
	"down")
		xbacklight -dec 5
		;;
	"default")
		xbacklight -set 50
		;;
	*)
		;;
esac
kill -38 "$(pidof dwmblocks)" 2> /dev/null
