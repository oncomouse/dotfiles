#!/usr/bin/env bash
winname="$(ratpoison -c windows | grep "[0-9]\+\*" | cut -d "*" -f 2)"
if [ "$winname" != "" ]; then
	echo "[$winname]"
else
	echo ""
fi
