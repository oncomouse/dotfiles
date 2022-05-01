local notifications = require("notifications")
local icons = require("icons")

local notif
local first_time = true
local timeout = 1.5

awesome.connect_signal("dotfiles::brightness::update", function(percentage)
	if first_time then
		first_time = false
	else
		local icon = icons.image.brightness
		-- Send notification
		notif = notifications.notify_dwim({
			title = "Brightness",
			message = tostring(percentage),
			icon = icon,
			timeout = timeout,
			app_name = "brightness",
		}, notif)
	end
end)
