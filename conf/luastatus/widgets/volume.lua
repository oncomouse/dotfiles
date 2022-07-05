widget = {
    plugin = "pulse",
    cb = function(t)
		local percent = t.cur / t.norm * 100
		if t.mute == "muted" then
			return "[婢]"
		end
		return string.format("[%s%0.0f%%]", "墳 ", percent)
	end,
}
