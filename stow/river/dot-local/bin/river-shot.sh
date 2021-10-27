#!/usr/bin/env bash

if [ "$1" == "full" ] || [ "$1" == "" ]; then
	grim "$(xdg-user-dir PICTURES)/$(date +'%s_grim.png')"
else
	grim -g "$(slurp)" "$(xdg-user-dir PICTURES)/$(date +'%s_grim.png')"
fi
