local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local recolor_icons = require("widgets.utils.recolor-icons")

local clock_widget = {}

local function create()
	local clock_extend_timeout = 5
	local formats = {
		regular = "%a %l:%M %p",
		extended = "%A, %B %d %Y",
	}
	local icons = recolor_icons({
		"clock",
		"calendar",
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
		extended = false,
	}
	clock_widget.extended = false
	clock_widget.restore = function()
		clock_widget.extended = false
		clock_widget:get_children_by_id("clock")[1].format = formats.regular
		clock_widget:get_children_by_id("image")[1]:set_image(icons["clock"])
	end
	clock_widget.extend = function()
		clock_widget.extended = true
		clock_widget:get_children_by_id("clock")[1].format = formats.extended
		clock_widget:get_children_by_id("image")[1]:set_image(icons["calendar"])
		gears.timer.start_new(clock_extend_timeout, clock_widget.restore)
	end
	clock_widget.toggle = function()
		if clock_widget.extended then
			clock_widget.restore()
		else
			clock_widget.extend()
		end
	end
	clock_widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			clock_widget.toggle()
		end
	end)

	return clock_widget
end

return setmetatable(clock_widget, { __call = function(_, ...)
	return create(...)
end })
