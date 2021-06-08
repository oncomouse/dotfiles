#!/usr/bin/env bash
source ~/.cache/wal/colors.sh

killall -q lemonbar
killall -q conky
# Wait until the processes have been shut down
# while pgrep -u $UID -x lemonbar >/dev/null; do sleep 1; done

fifo="/tmp/panel_fifo"
[ -e "$fifo" ] && rm "$fifo"
mkfifo "$fifo"

conky > "$fifo" &
xtmon > "$fifo" &

while read -r line; do
	case $line in
		focus_changed*)
			title="${line:25}"
			id="${line:15:8}"
			;;
		title_changed*)
			l_id="${line:15:8}"
			if [ "$l_id" == "$id" ]; then
				id="$l_id"
				title="${line:25}"
			fi
			;;
		initial_focus*)
			title="${line:25}"
			id="${line:15:8}"
			;;
		removed_*)
			;;
		new_window*)
			;;
		initial*)
			;;
		*)
			conky="$line"
			;;
	esac
	if [ "${#title}" -ge 80  ]; then
		echo "$conky %{c}${title:0:79}…"
	else
		echo "$conky %{c}$title"
	fi
done < "$fifo" | lemonbar -f "FiraCode Nerd Font:size=9" -B "$color0" -F "$color7" | sh
