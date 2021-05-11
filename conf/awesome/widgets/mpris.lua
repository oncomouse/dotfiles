-- luacheck: globals awesome
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

-- Track current player, for multi-player support:
local current_player = nil
local get_current_player = function()
	awful.spawn.easy_async('bash -c \'for p in $(playerctl -l); do if [ "$(playerctl -p $p status)" == "Playing" ]; then echo $p; fi; done\'', function(player)
		if player ~= '' then
			current_player = player
		end
	end)
end
gears.timer {
	timeout   = 0.5,
	call_now  = true,
	autostart = true,
	callback  = get_current_player,
}
-- Truncate:
function truncate(st, len)
	return string.len(st) > len and string.sub(st, 0, len - 1) .. "…" or st
end
-- Custom playerctl guts:
local playerctl = {
	play = function()
		local switch = ""
		if current_player ~= nil then
			switch = " -p " .. current_player
		end
		awful.spawn.easy_async("playerctl" .. switch .. " play-pause", get_current_player)
	end,
	stop = function()
		local switch = ""
		if current_player ~= nil then
			switch = " -p " .. current_player
		end
		awful.spawn.easy_async("playerctl" .. switch .. " stop", function()
			current_player = nil
		end)
	end,
	previous_track = function()
		local switch = ""
		if current_player ~= nil then
			switch = " -p " .. current_player
		end
		awful.spawn("playerctl" .. switch .. " previous")
	end,
	next_track = function()
		local switch = ""
		if current_player ~= nil then
			switch = " -p " .. current_player
		end
		awful.spawn("playerctl" .. switch .. " next")
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
-- Metadata
local icon
local artist
local album
local title
local artUrl
local parse_metadata = function(stdout)
	local parts = gears.string.split(stdout, "::")
	return table.unpack(parts)
end
gears.timer{
	timeout   = 0.25,
	call_now  = true,
	autostart = true,
	callback  = function()
		local switch = ""
		if current_player ~= nil then
			switch = " -p " .. current_player
		end
		awful.spawn.easy_async(
			"playerctl" ..
			switch ..
			" -f '{{status}}::{{artist}}::{{title}}::{{album}}::{{mpris:artUrl}}' metadata",
			function(metadata)
				status, artist, title, album, artUrl = parse_metadata(metadata)
				if status == "Playing" then
					icon = "契"
				elseif status == "Paused" then
					icon = ""
				else
					icon = "栗"
				end
				mpris_widget:get_children_by_id("title")[1]:set_text(
					artist ~= nil and truncate(string.format("%s %s - %s", icon, artist, title), 30) or ""
				)
			end
		)
	end,
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
local mpris_tooltip = awful.tooltip {
	mode = 'outside',
	preferred_positions = {'bottom'},
 }
mpris_tooltip:add_to_object(mpris_widget)
mpris_widget:connect_signal('mouse::enter', function()
	if artist ~= nil then
		mpris_tooltip.markup = '<b>Artist</b>: ' .. artist
			.. '\n<b>Song</b>: ' .. title
			.. '\n<b>Album</b>: ' .. album
	end
end)
return mpris_widget
