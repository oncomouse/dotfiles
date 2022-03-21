#!/usr/bin/env bash

status="$(dotfiles-media status)"
if [ ${#status} -gt 50 ]; then
	echo "<txt>$(echo "$status" | cut -c1-49)…</txt>"
else
	echo "<txt>$status</txt>"
fi
echo "<txtclick>dotfiles-media play</txtclick>"
