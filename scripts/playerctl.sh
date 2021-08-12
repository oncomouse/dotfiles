#!/usr/bin/env bash

players="mpd,mopidy,ncspot,mpv"

print_mpris() {
	local line="$1"
	echo "$line" | sed -e "s/Playing::/契 /" -e "s/Paused::/ /" -e "s/Stopped::/栗 /" 
}

print_and_exit() {
	local line="$(playerctl -p "$players" --format "{{status}}::{{artist}} - {{title}}" metadata 2> /dev/null)"
	print_mpris "$line"
	exit
}

if [ "$BUTTON" = 1 ] || [ "$1" = "play" ]; then
	playerctl play-pause
	print_and_exit
elif [ "$BUTTON" = 2 ] || [ "$1" = "previous" ]; then
	playerctl previous
	print_and_exit
elif [ "$BUTTON" = 3 ] || [ "$1" = "next" ]; then
	playerctl next
	print_and_exit
elif [ "$1" == "stop" ]; then
	playerctl stop
	print_and_exit
fi

playerctlfifo="/tmp/playerctl"
[ -e "$playerctlfifo" ] && rm "$playerctlfifo"
mkfifo "$playerctlfifo"

playerctl -p "$players" --follow --format "{{status}}::{{artist}} - {{title}}" metadata 2> /dev/null > "$playerctlfifo" &

while read -r line ; do
	print_mpris "$line"
done < "$playerctlfifo"
