-- luacheck: globals awesome
local lgi = require("lgi")
local awful = require("awful")
local gears = require("gears")

local function evil_init()
	local Playerctl = lgi.Playerctl
	local manager = lgi.Playerctl.PlayerManager()

	-- Mpris player status:
	local current_player = nil

	local supported_players = {
		'mpd',
		'ncspot'
	}
	local status = "STOPPED"
	local icon
	local artist
	local album
	local title
	local artUrl

	function status_to_icon()
		if status == "PLAYING" then
			icon = "契"
		elseif status == "PAUSED" then
			icon = ""
		else
			icon = "栗"
		end
	end

	function signal_update()
		status_to_icon()
		awesome.emit_signal("evil::mpris_widget::metadata", icon, artist, title, album, artUrl)
	end

	function on_playback_status(player, new_status, _)
		status = new_status
		if status == "STOPPED" then
			current_player = manager.players[1]
		elseif status == "PLAYING" then
			current_player = player
		end
		signal_update()
	end

	function on_metadata(player, _, _)
		artist = player:get_artist()
		album = player:get_album()
		title = player:get_title()
		for k, v in player.metadata:pairs() do
			if k:match(".*:artUrl") and v.value:sub(1,8) == 'https://' then
				if artUrl ~= v.value then
					artUrl = v.value
					awful.spawn.with_shell("curl -s " .. artUrl .." | convert - ~/.cache/awesome/mpris.png ")
				end
			end
			if k:match(".*:artUrl") and v.value:sub(1, 8) == "file:///" then
				artUrl = v.value:sub(7)
			end
		end
		signal_update()
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
				on_metadata(current_player)
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

	function manager.on_name_appeared(name)
		follow_player(name)
	end

	manager.on_name_appeared:connect("name-appeared")

	awesome.connect_signal("evil::mpris_widget::change", function(action)
		if action == "play_pause" then
			current_player:play_pause()
		elseif action == "previous" then
			current_player:previous()
		elseif action == "next" then
			current_player:next()
		elseif action == "stop" then
			current_player:stop()
		end
	end)
end

return evil_init
