widget = {
    plugin = "pulse",
    cb = function(t)
		local percent = t.cur / t.norm * 100
		if t.mute then
			return "[^ca(1, dotfiles-media mute)婢^ca()]"
		end
		return string.format("[^ca(1, dotfiles-media mute, 4, dotfiles-media up, 5, dotfiles-media down)%s%0.0f%%^ca()]", "墳 ", percent)
	end,
}
