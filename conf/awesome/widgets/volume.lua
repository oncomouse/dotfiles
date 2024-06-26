local awful = require("awful")
local Block = require("widgets.block")

return Block({
	name = "volume",
	buttons = {
		[awful.button.names.LEFT] = function()
			awful.spawn.with_shell("dotfiles-media mute", false)
		end,
		[awful.button.names.SCROLL_UP] = function()
			awful.spawn.with_shell("dotfiles-media volume up", false)
		end,
		[awful.button.names.SCROLL_DOWN] = function()
			awful.spawn.with_shell("dotfiles-media volume down", false)
		end,
	},
	callback = "dotfiles::volume::request",
	signals = {
		["dotfiles::volume::update"] = function(update, volume, muted)
			local output
			if muted then
				output = "婢" .. " x"
			else
				output = "墳 " .. tostring(volume) .. "%"
			end
			update(output)
		end,
	},
})
