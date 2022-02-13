local wibox = require("wibox")
local Block = require("widgets.block")

return Block({
	widget = wibox.widget.systray,
	margins = {
		top = 2,
		bottom = 2,
	},
})
