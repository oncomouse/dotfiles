#!/usr/bin/env bash
case $BUTTON in
	1) river-volume.sh mute ;;
	4) river-volume.sh up ;;
	5) river-volume.sh down ;;
esac
icon="墳"
output="$(pamixer --get-volume-human | sed s/muted/x/g)"
if [ ${#output} -gt 0 ]; then
	echo "$icon $output"
fi
