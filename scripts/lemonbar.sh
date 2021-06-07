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
			;;
		initial_focus*)
			title="${line:25}"
			;;
		initial*)
			;;
		*)
			conky="$line"
			;;
	esac
	echo "$conky %{c}$title"
done < "$fifo" | lemonbar -f "FiraCode Nerd Font:size=9" -B "$color0" -F "$color7" | sh
