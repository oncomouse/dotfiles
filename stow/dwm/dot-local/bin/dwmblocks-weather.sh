#!/usr/bin/env bash

case $BUTTON in
	1) xdg-open "https://wttr.in" ;;
	*) curl --connect-timeout 40 -s "https://wttr.in/$(cat ~/.wttr.location 2> /dev/null)?format=1&u" | sed -e "s/ +//" -e "s/Unknown.*\$//"
esac
