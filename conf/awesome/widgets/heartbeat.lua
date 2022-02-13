-- luacheck: globals awesome
local awful = require("awful")
local Block = require("widgets.block")

local status = {
	on = "",
	off = "﯈",
}
return Block({
	buttons = {
		[awful.button.names.LEFT] = function()
			awesome.emit_signal("dotfiles::heartbeat::toggle")
		end,
	},
	widget_options = {
		forced_width = 20,
		align = "center",
	},
	signals = {
		["dotfiles::heartbeat::update"] = function(update, on)
			local output = on and status.on or status.off
			update(output)
		end,
	},
	callback = "dotfiles::heartbeat::request",
})
