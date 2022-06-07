#!/usr/bin/env bash

music_dir=/mnt/Media/Music
if [ ! -d "$music_dir" ]; then
	music_dir="$HOME/Music"
fi

dirs="$(fd -t f -e flac . "$music_dir" -x echo {//}  | sort | uniq)"

IFS=$'\n'
mkdir -p "$HOME/My Music/out"
for dir in $(printf "%s" "$dirs" | fzf --reverse -m |  sed -e "s/\n/ /g"); do
	fd -t f -e flac . "$dir" -x ffmpeg -i "{}" -qscale:a 0 "$HOME/My Music/out/{/.}.mp3"
done
