local wibox = require("wibox")
local gt = require("gears.timer")

local clock_extend_timeout = 5
local clock_widget = wibox.widget{
	widget = wibox.widget.textclock,
	format = " %a %l:%M %p ",
	extended = false,
	restore = function(self)
		self.extended = false
		self.format = " %a %l:%M %p "
	end,
	extend = function(self)
		self.extended = true
		self.format = "  %A, %B %d %Y "
		gt.start_new(clock_extend_timeout, self.restore)
	end,
	toggle = function(self)
		if self.extended then
			self.restore()
		else
			self.extend()
		end
	end,
}
clock_widget:connect_signal("button::press", clock_widget.toggle)

return clock_widget
