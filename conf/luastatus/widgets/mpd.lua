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
			return string.format("[%s%s - %s]", state, t.song.Artist, t.song.Title)
		end
	end
}
