#!/usr/bin/env bash
if command -v acpi > /dev/null; then
	icon=""
	charging="ﮣ"
	status="$(acpi -b 2> /dev/null)"
	output="$(echo "$status" | cut -d , -f 2 | sed 's/^ *//g')"
	if echo "$status" | grep -q Discharging; then
		charging="ﮤ"
	elif echo "$status" | grep -q Charging; then
		icon=""
	fi
	if [ ${#output} -gt 0 ]; then
		echo "[$icon$output$charging]"
	fi
else
	echo ""
fi
