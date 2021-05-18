-- luacheck: globals awesome mouse
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local lgi = require("lgi")

local Playerctl = lgi.Playerctl
local manager = lgi.Playerctl.PlayerManager()

-- Mpris player status:
local current_player = nil
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
		current_player:play_pause()
	end,
	previous_track = function()
		current_player:previous()
	end,
	next_track = function()
		current_player:next()
	end,
	stop = function()
		current_player:stop()
	end,
}

local supported_players = {
	'mpd',
	'ncspot'
}
local icon
local artist
local album
local title
local artUrl
-- Truncate:
function truncate(st, len)
	return string.len(st) > len and string.sub(st, 0, len - 1) .. "…" or st
end

local status = "STOPPED"
function on_playback_status(player, new_status, _)
	status = new_status
	if status == "STOPPED" then
		current_player = manager.players[1]
	elseif status == "PLAYING" then
		current_player = player
	end
end

function on_metadata(player, _, _)
	artist = player:get_artist()
	album = player:get_album()
	title = player:get_title()
	if status == "PLAYING" then
		icon = "契"
	elseif status == "PAUSED" then
		icon = ""
	else
		icon = "栗"
	end
	for k, v in player.metadata:pairs() do
		if k:match(".*:artUrl") and v.value:sub(1,8) == 'https://' then
			if artUrl ~= v.value then
				artUrl = v.value
				awful.spawn.with_shell("curl " .. artUrl .." | convert - ~/.cache/awesome/mpris.png ")
			end
		end
		if k:match(".*:artUrl") and v.value:sub(1, 8) == "file:///" then
			artUrl = v.value:sub(7)
		end
	end

	mpris_widget:get_children_by_id("title")[1]:set_text(
		artist ~= nil and truncate(string.format("%s %s - %s", icon, artist, title), 30) or ""
	)
end

function follow_player(name)
	if gears.table.hasitem(supported_players, gears.string.split(name.name, ".")[1]) then
		player = Playerctl.Player.new_from_name(name)
		player.on_playback_status = on_playback_status
		player.on_metadata = on_metadata
		manager:manage_player(player)

		-- If a player is current player, it is our current player:
		if player.playback_status == "PLAYING" then
			current_player = player
			status = "PLAYING"
		end
	end
	return nil
end

for i, name in ipairs(Playerctl.list_players()) do
	if i == 1 then
		current_player = Playerctl.Player.new_from_name(name)
	end
	follow_player(name)
end

function manager:on_name_appeared(name)
	follow_player(name)
end

manager.on_name_appeared:connect("name-appeared")

-- Metadata
mpris_widget:connect_signal("button::press", function(self, _, _, button)
	if button == 1 then
		self:play()
	elseif button == 2 then
		self:previous()
	elseif button == 3 then
		self:next()
	end
end)
local mpris_tooltip = awful.tooltip {
	mode = 'outside',
	preferred_positions = {'bottom'},
 }
mpris_tooltip:add_to_object(mpris_widget)

local mpris_popup = awful.popup{
	ontop = true,
	visible = false,
	shape = gears.shape.rect,
	-- border_width = 1,
	-- border_color = beautiful.bg_focus,
	maximum_width = 400,
	offset = {y = 5},
	widget = {},
	set_text = function(self, text)
		self:get_children_by_id("text")[1]:set_markup(text)
	end
}

local function get_image()
	if artUrl == nil then
		return ''
	end
	if artUrl:sub(1,8) == 'https://' then
		return os.getenv('HOME') .. '/.cache/awesome/mpris.png'
	end
	return artUrl
end
mpris_widget:connect_signal('mouse::enter', function()
	if artist ~= nil then
		-- mpris_tooltip.markup = '<b>Artist</b>: ' .. artist
		-- 	.. '\n<b>Song</b>: ' .. title
		-- 	.. '\n<b>Album</b>: ' .. album
		mpris_popup:setup{
			{
				widget=wibox.widget.imagebox,
				id="image",
				image= get_image(),
				forced_width=50,
			},
			{
				widget=wibox.widget.textbox,
				id="text",
				markup='<b>Artist</b>: ' .. artist
				.. '\n<b>Song</b>: ' .. title
				.. '\n<b>Album</b>: ' .. album,
			},
			layout  = wibox.layout.fixed.horizontal,
		}
	end
	mpris_popup.visible = true
	mpris_popup:move_next_to(mouse.current_widget_geometry)
end)
mpris_widget:connect_signal('mouse::leave', function()
	mpris_popup.visible = false
end)

return mpris_widget
