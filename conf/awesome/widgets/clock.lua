local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local recolor_icons = require("widgets.utils.recolor-icons")

local clock_widget = {}

local function create()
	local formats = {
		regular = "%a %m/%d %-I:%M %p ",
	}
	local icons = recolor_icons({
		"clock",
	}, function(x) return "apps/scalable/"..x..".svg" end)
	clock_widget = wibox.widget{
		{
			{
				{
					id = "image",
					image = icons["clock"],
					widget = wibox.widget.imagebox,
					forced_width = beautiful.icon_size,
					forced_height = beautiful.icon_size,
				},
				widget = wibox.container.place,
			},
			{
				widget = wibox.widget.textclock,
				format = formats.regular,
				id = "clock"
			},
			layout = wibox.layout.fixed.horizontal,
		},
		widget = wibox.container.margin,
	}

	return clock_widget
end

return setmetatable(clock_widget, { __call = function(_, ...)
	return create(...)
end })
