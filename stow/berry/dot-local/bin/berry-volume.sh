#!/usr/bin/env bash

# vol_change=${2:-5}
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
kill -37 $(pidof dwmblocks)
