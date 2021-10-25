#!/usr/bin/env bash

source ~/.cache/wal/colors.sh
pkill -f swayidle
exec swayidle -w \
	timeout 300 "swaylock -c ${color0//#/}" \
	before-sleep "swaylock -c ${color0//#/}" \
# resume "wlopm --on $(wlopm | sed -e "s/ .*//")"
