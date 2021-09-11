#!/usr/bin/env bash
icon=""
output="$(acpi 2> /dev/null | cut -d , -f 2 | sed 's/^ *//g')"
if acpi -b | grep -q Discharging; then
	charging="ﮤ"
else
	charging="ﮣ"
	icon=""
fi
if [ ${#output} -gt 0 ]; then
	echo "$icon $output $charging"
fi
