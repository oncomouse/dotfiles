local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local clock_popup = wibox({
	visible = false,
	ontop = true,
	type = "dock",
	screen = screen.primary,
	shape = gears.shape.rounded_rect,
})
clock_popup.height = 75
clock_popup.width = 720
clock_popup.bg = beautiful.color0 .. "CC"
clock_popup.fg = beautiful.color15
awful.placement.centered(clock_popup)

clock_popup:buttons(gears.table.join(awful.button({}, 1, function()
	clock_popup:toggle(false)
end)))
clock_popup:setup({
	{
		{
			widget = wibox.widget.textclock,
			format = "%a %m/%d %-I:%M %p",
			font = "FiraCode Nerd Font 48",
			align = "left",
		},
		layout = wibox.layout.align.horizontal,
	},
	expand = "none",
	layout = wibox.layout.align.vertical,
})
local clock_timer = gears.timer({
	timeout = 10,
	call_now = false,
	autostart = false,
	callback = function()
		clock_popup:toggle(false)
	end,
})
function clock_popup:toggle(target)
	self.visible = target or not self.visible
	if self.visible then
		clock_timer:start()
	else
		clock_timer:stop()
	end
end

return clock_popup
