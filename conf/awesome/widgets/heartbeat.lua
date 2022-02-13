-- luacheck: globals awesome
local Block = require("widgets.block")

local status = {
	on = "",
	off = "﯈",
}
return Block({
	buttons = {
		[1] = function()
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
	cb = "dotfiles::heartbeat::request",
})
