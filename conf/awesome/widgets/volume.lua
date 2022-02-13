-- luacheck: globals awesome
local awful = require("awful")
local Block = require("widgets.block")

return Block({
	name = "volume",
	buttons = {
		[1] = function() awful.spawn.with_shell("liskin-media mute", false) end,
		[4] = function() awful.spawn.with_shell("liskin-media volume up", false) end,
		[5] = function() awful.spawn.with_shell("liskin-media volume down", false) end,
	},
	cb = "dotfiles::volume::request",
	timeout = 30,
	signals = {
		["dotfiles::volume::update"] = function(update, volume, muted)
			local output
			if muted then
				output = "婢" .. " x"
			else
				output = "墳 " .. tostring(volume) .. "%"
			end
			update(output)
		end
	}
})
