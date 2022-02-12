-- luacheck: globals awesome client
-- Source: https://github.com/elenapan/dotfiles/blob/master/config/awesome/notifications/volume.lua
local notifications = require("notifications")

local icons = {
	image = {
		volume = "/usr/share/icons/ePapirus-Dark/24x24/panel/audio-volume-high.svg",
		muted = "/usr/share/icons/ePapirus-Dark/24x24/panel/audio-volume-muted.svg",
	},
}

local notif
local timeout = 1.5
local first_time = true
awesome.connect_signal("evil::volume", function(percentage, muted)
	if first_time then
		first_time = false
	else
		if client.focus and client.focus.class == "Pavucontrol" then
			-- Sidebar and Pavucontrol already show volume, so
			-- destroy notification if it exists
			if notif then
				notif:destroy()
			end
		else
			-- Send notification
			local message, icon
			if muted then
				message = "muted"
				icon = icons.image.muted
			else
				message = tostring(percentage) .. "%"
				icon = icons.image.volume
			end

			notif = notifications.notify_dwim(
				{ title = "Volume", message = message, icon = icon, timeout = timeout, app_name = "volume" },
				notif
			)
		end
	end
end)
