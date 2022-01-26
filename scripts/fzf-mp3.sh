#!/usr/bin/env bash

dirs=$(cat <<'EOF' | lua
local function has(self, item)
	for _,i in pairs(self) do
		if i == item then
			return true
		end
	end
	return false
end

function string:split(pat)
	pat = pat or "%s+"
	local st, g = 1, self:gmatch("()(" .. pat .. ")")
	local function getter(segs, seps, sep, cap1, ...)
		st = sep and seps + #sep
		return self:sub(segs, (seps or 0) - 1), cap1 or sep, ...
	end
	return function()
		if st then
			return getter(st, g())
		end
	end
end

local handle = io.popen("fd -t f -e flac . /mnt/Media/Music -x echo {//}")
local results = handle:read("*a")
handle:close()

local directories = {}
for line in results:split("\n") do
	if not has(directories, line) then
		table.insert(directories, line)
		print(line)
	end
end

EOF
)

IFS=$'\n'
for dir in $(printf "%s" "$dirs" | fzf --reverse -m |  sed -e "s/\n/ /g"); do
	fd -t f -e flac . "$dir" -x ffmpeg -i "{}" -qscale:a 0 "$HOME/My Music/out/{/.}.mp3"
done
