widget = {
	plugin = "mpd",
	cb = function(t)
		if t.what == "update" then
			if t.status.state == "stop" then
				return ""
			end
			local state = "契 "
			if t.status.state == "pause" then
				state=" "
			end
			local output = string.format("%s%s - %s", state, t.song.Artist, t.song.Title)
			if #output > 40 then
				output = string.sub(output, 1, 39) .. "…"
			end
			return string.format("[^ca(1, dotfiles-media play, 2, dotfiles-media prev, 3, dotfiles-media next)%s^ca()]", output)
		end
	end
}
