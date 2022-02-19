-- Source: https://github.com/elenapan/dotfiles/blob/master/config/awesome/notifications/mpd.lua
local icons = require("icons")
local notifications = require("notifications")

notifications.mpris = {}

local notif
local first_time = true
local timeout = 2

local old_update
local send_mpris_notif = function(artist, song, paused)
	if first_time then
		first_time = false
	else
		if paused or (client.focus and (client.focus.instance == "music" or client.focus.class == "music")) then
			-- Sidebar and already shows mpris info, so
			-- destroy notification if it exists
			-- Also destroy it if music pauses
			if notif then
				notif:destroy()
			end
		else
			if artist .. song ~= old_update then
				notif = notifications.notify_dwim({
					title = "Now playing:",
					message = "<b>" .. song .. "</b> by <b>" .. artist .. "</b>",
					icon = icons.image.music,
					timeout = timeout,
					app_name = "mpris",
				}, notif)
			end
		end
		old_update = artist .. song
	end
end

awesome.connect_signal("dotfiles::mpris::update", function(status, metadata)
	awesome.emit_signal("dotfiles::mpris::notify", metadata.artist or "", metadata.title or "", status == "PAUSED")
end)

-- Allow dynamically toggling mpd notifications
notifications.mpris.enable = function()
	awesome.connect_signal("dotfiles::mpris::notify", send_mpris_notif)
	notifications.mpris.enabled = true
end
notifications.mpris.disable = function()
	awesome.disconnect_signal("dotfiles::mpris::notify", send_mpris_notif)
	notifications.mpris.enabled = false
end
notifications.mpris.toggle = function()
	if notifications.mpris.enabled then
		notifications.mpris.disable()
	else
		notifications.mpris.enable()
	end
end

-- Start with mpris notifications enabled
notifications.mpris.enable()
