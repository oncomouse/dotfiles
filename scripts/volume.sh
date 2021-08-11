#!/usr/bin/env bash

function change {
	local vol_change=${2:-5}
	case "$1" in
	  "up")
		if pamixer --get-mute > /dev/null; then
		  pamixer -u
		else
		  pamixer -i "$vol_change"
		fi
		;;
	  "down")
		if pamixer --get-mute > /dev/null; then
		  pamixer -u
		else
		  pamixer -d "$vol_change"
		fi
		;;
	  "mute")
		pamixer -t > /dev/null
		;;
	  *)
		;;
	esac
}
case $BUTTON in
	1) change mute ;;
	4) change down ;;
	5) change up ;;
esac
case $1 in
	up) change up ;;
	down) change down ;;
	mute) change mute ;;
esac
icon="墳"
output="$(pamixer --get-volume-human | sed s/muted/x/g)"
if [ ${#output} -gt 0 ]; then
	echo "$icon $output"
fi
