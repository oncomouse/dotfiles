#!/usr/bin/env bash

case $BUTTON in
	1) xdg-open "https://wttr.in" ;;
	*) curl -s 'https://wttr.in/?format=1' | sed -e "s/ +//" -e "s/Unknown.*\$//"
esac
