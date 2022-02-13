-- luacheck: globals awesome
local awful = require("awful")
local Block = require("widgets.block")

return Block({
	buttons = {
		[awful.button.names.LEFT] = function()
			awesome.emit_signal("dotfiles::action", "default")
		end,
		[awful.button.names.SCROLL_UP] = function()
			awesome.emit_signal("dotfiles::action", "down")
		end,
		[awful.button.names.SCROLL_DOWN] = function()
			awesome.emit_signal("dotfiles::action", "up")
		end,
	},
	cb = "dotfiles::brightness::request",
	signals = {
		"dotfiles::brightness::update", function(update, brightness)
			update("" .. tostring(brightness) .. "%")
		end
	},
})
