#!/usr/bin/env bash
case $BUTTON in
	1) dotfiles-volume mute ;;
	4) dotfiles-volume up ;;
	5) dotfiles-volume down ;;
esac
icon="墳"
output="$(pamixer --get-volume-human | sed s/muted/x/g)"
if [ ${#output} -gt 0 ]; then
	echo "$icon $output"
fi
