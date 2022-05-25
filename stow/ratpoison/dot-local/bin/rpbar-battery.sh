#!/usr/bin/env bash
if command -v acpi > /dev/null; then
	icon=""
	charging="ﮣ"
	output="$(acpi 2> /dev/null | cut -d , -f 2 | sed 's/^ *//g')"
	if acpi -b | grep -q Discharging; then
		charging="ﮤ"
	else
		icon=""
	fi
	if [ ${#output} -gt 0 ]; then
		echo "[$icon $output$charging]"
	fi
else
	echo ""
fi
