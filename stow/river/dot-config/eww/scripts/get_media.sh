#!/usr/bin/env bash
media=$(playerctl -p spotify metadata -f "{{title}}")
player_status=$(playerctl -p spotify status)

if [[ $player_status = "Paused" ]]
then
    song_status=' '
	echo -e "$song_status $media"
elif [[ $player_status = "Playing" ]]
then
    song_status=' '
	echo -e "$song_status $media"
else
    song_status=''
	echo -e "$song_status"
fi

