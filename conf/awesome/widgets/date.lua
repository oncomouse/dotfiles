local wibox = require("wibox")
local Block = require("widgets.block")

return Block({
	name = "date",
	widget = wibox.widget({
		{
			format = "%a %m/%d %-I:%M %p ",
			widget = wibox.widget.textclock
		},
		layout = wibox.layout.fixed.horizontal,
	}),
})
