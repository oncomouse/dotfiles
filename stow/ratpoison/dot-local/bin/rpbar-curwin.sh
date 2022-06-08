#!/usr/bin/env bash
winname="$(ratpoison -c windows | grep "[0-9]\+\*" | cut -d "*" -f 2)"
if [ "$winname" != "" ]; then
	if [ ${#winname} -gt 90 ]; then
		winname="${winname:0:89}…"
	fi
	echo "[$winname]"
else
	echo ""
fi
