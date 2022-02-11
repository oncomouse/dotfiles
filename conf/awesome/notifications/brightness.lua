-- luacheck: globals awesome
local notifications = require("notifications")

local notif
local first_time = true
local timeout = 1.5

awesome.connect_signal("evil::brightness", function(percentage)
	if first_time then
		first_time = false
	else
		-- Send notification
		notif = notifications.notify_dwim(
			{ title = "Brightness", message = tostring(percentage), timeout = timeout, app_name = "brightness" },
			notif
		)
	end
end)
