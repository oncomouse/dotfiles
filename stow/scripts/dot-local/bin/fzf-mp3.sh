#!/usr/bin/env bash

music_dir=/mnt/Media/Music
if [ ! -d "$music_dir" ]; then
	music_dir="$HOME/Music"
fi

dirs="$(fd -t f -e flac . "$music_dir" -x echo {//}  | sort | uniq)"

IFS=$'\n'
for dir in $(printf "%s" "$dirs" | fzf --reverse -m |  sed -e "s/\n/ /g"); do
	output_dir="$HOME/My Music/out/$(echo "$dir" | awk -F/ '{printf("%s/%s", $(NF-1), $(NF))}')"
	mkdir -p "$output_dir"
	fd -t f -e flac . "$dir" -x ffmpeg -i "{}" -qscale:a 0 "$output_dir/{/.}.mp3"
done
