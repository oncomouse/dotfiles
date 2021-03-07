local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")

local clock_widget = wibox.widget.textclock(" %a %l:%M %p ")
clock_widget:connect_signal("button::press", function()
	awful.screen.focused().right_panel:toggle()
	-- awful.spawn.easy_async('date +"  %A, %B %d %Y"', function(stdout)
	-- 	naughty.notify({
	-- 		title = "Today's Date",
	-- 		text = string.gsub(stdout, "^%s*(.-)%s*$", "%1"),
	-- 	})
	-- end)
end)

return clock_widget
