local awful = require("awful")
local Block = require("widgets.block")

return Block({
	buttons = {
		[awful.button.names.LEFT] = function()
			awesome.emit_signal("dotfiles::brightness::action", "default")
		end,
		[awful.button.names.SCROLL_UP] = function()
			awesome.emit_signal("dotfiles::brightness::action", "up")
		end,
		[awful.button.names.SCROLL_DOWN] = function()
			awesome.emit_signal("dotfiles::brightness::action", "down")
		end,
	},
	callback = "dotfiles::brightness::request",
	signals = {
		["dotfiles::brightness::update"] = function(update, brightness)
			update(" " .. tostring(brightness) .. "%")
		end
	},
})
