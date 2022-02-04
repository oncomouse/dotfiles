#!/usr/bin/env bash

status="$(liskin-media status)"
if [ ${#status} -gt 50 ]; then
	echo "<txt>$(echo "$status" | cut -c1-49)…</txt>"
else
	echo "<txt>$status</txt>"
fi
echo "<txtclick>liskin-media play</txtclick>"
