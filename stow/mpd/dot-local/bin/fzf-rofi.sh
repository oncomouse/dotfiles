#!/usr/bin/env bash

match="$(mpc playlist -f "%position%. %artist% - %title%" | fzf --no-sort | cut -d "." -f 1)"
if [[ "$match" != "" ]]; then
	mpc play "$match" >/dev/null
fi
