local wibox = require("wibox")
local gt = require("gears.timer")

local clock_extend_timeout = 5
local clock_widget = wibox.widget.textclock(" %a %l:%M %p ")
clock_widget.extended = false
clock_widget.restore = function()
	clock_widget.extended = false
	clock_widget.format = " %a %l:%M %p "
end
clock_widget.extend = function()
	clock_widget.extended = true
	clock_widget.format = "  %A, %B %d %Y "
	gt.start_new(clock_extend_timeout, clock_widget.restore)
end
clock_widget.toggle = function()
	if clock_widget.extended then
		clock_widget.restore()
	else
		clock_widget.extend()
	end
end
clock_widget:connect_signal("button::press", clock_widget.toggle)

return clock_widget
