-- luacheck: globals awesome playerctl
local wibox = require("wibox")
-- Custom playerctl guts:
local playerctl = require("signals.playerctl")
playerctl.enable()
-- Mpris player status:
local mpris_widget = wibox.widget{
	{
		{
			id = "status",
			text = "栗",
			widget = wibox.widget.textbox,
		},
		{
			id = "title",
			text = "",
			widget = wibox.widget.textbox,
		},
		layout = wibox.layout.fixed.horizontal,
	},
	margin2 = 1,
	widget = wibox.container.margin,
	play = function()
		playerctl.play()
	end,
	previous_track = function()
		playerctl.previous_track()
	end,
	next_track = function()
		playerctl.next_track()
	end,
	stop = function()
		playerctl.stop()
	end
}
mpris_widget:connect_signal("button::press", function(_, _, _, button)
	if button == 1 then
		playerctl.play()
	elseif button == 2 then
		playerctl.previous_track()
	elseif button == 3 then
		playerctl.next_track()
	end
end)
function truncate(st, len)
	if string.len(st) > len then
		return string.sub(st, 0, len - 1) .. "…"
	else
		return st
	end
end
awesome.connect_signal("dotfiles::playerctl::stopped", function()
	mpris_widget:get_children_by_id("status")[1]:set_text("栗")
	mpris_widget:get_children_by_id("title")[1]:set_text("")
end)
awesome.connect_signal("dotfiles::playerctl::status", function(playing)
	local status
	if playing == "play" then
		status = "契 "
	elseif playing == "pause" then
		status = " "
	else
		status = "栗 "
	end
	mpris_widget:get_children_by_id("status")[1]:set_text(status)
end)
awesome.connect_signal("dotfiles::playerctl::title_artist_album", function(
title,
	artist,
	_
)
	mpris_widget:get_children_by_id("title")[1]:set_text(
		truncate(string.format("%s - %s", artist, title), 30)
	)
end)

return mpris_widget
