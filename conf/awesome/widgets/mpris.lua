-- luacheck: globals awesome
local wibox = require("wibox")
local awful = require("awful")
-- Truncate:

function truncate(st, len)
	return string.len(st) > len and string.sub(st, 0, len - 1) .. "…" or st
end
-- Custom playerctl guts:
local playerctl = {
	play = function()
		awful.spawn("playerctl play-pause")
	end,
	stop = function()
		awful.spawn("playerctl stop")
	end,
	previous_track = function()
		awful.spawn("playerctl previous")
	end,
	next_track = function()
		awful.spawn("playerctl next")
	end,
}
-- Mpris player status:
local mpris_widget = wibox.widget{
	{
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
	end,
}
local mpris_status_pid = awful.spawn.with_line_callback(
	"sh -c '~/.local/share/polybar-scripts/polybar-scripts/player-mpris-tail/player-mpris-tail.py --icon-playing 契 --icon-paused  --icon-stopped 栗'-b firefox -b vlc",
	{ stdout = function(stdout)
		mpris_widget:get_children_by_id("title")[1]:set_text(
			truncate(stdout, 30)
		)
	end }
)
awesome.connect_signal("exit", function()
	if mpris_status_pid ~= nil then
		awesome.kill(mpris_status_pid, awesome.unix_signal.SIGTERM)
	end
end)
mpris_widget:connect_signal("button::press", function(_, _, _, button)
	if button == 1 then
		playerctl.play()
	elseif button == 2 then
		playerctl.previous_track()
	elseif button == 3 then
		playerctl.next_track()
	end
end)

return mpris_widget
