#!/usr/bin/env bash

mkdir -p "$HOME/Pictures"
if [ "$1" == "full" ] || [ "$1" == "" ]; then
	grim "$HOME/Pictures/$(date +'Screenshot-%s.png')"
else
	grim -g "$(slurp)" "$HOME/Pictures/$(date +'Screenshot-%s.png')"
fi
