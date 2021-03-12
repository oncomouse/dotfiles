local wibox = require("wibox")
local gt = require("gears.timer")
-- local awful = require("awful")
-- local naughty = require("naughty")

local clock_widget = wibox.widget.textclock(" %a %l:%M %p ")
clock_widget.extended = false
clock_widget.restore = function()
	clock_widget.extended = false
	clock_widget.format = " %a %l:%M %p "
end
clock_widget.extend = function()
	clock_widget.extended = true
	clock_widget.format = "  %A, %B %d %Y "
	gt.start_new(10, clock_widget.restore)
end
clock_widget:connect_signal("button::press", function()
	if clock_widget.extended then
		clock_widget.restore()
	else
		clock_widget.extend()
	end
	-- awful.screen.focused().right_panel:toggle()
	-- awful.spawn.easy_async('date +"  %A, %B %d %Y"', function(stdout)
	-- 	naughty.notify({
	-- 		title = "Today's Date",
	-- 		text = string.gsub(stdout, "^%s*(.-)%s*$", "%1"),
	-- 	})
	-- end)
end)

return clock_widget
