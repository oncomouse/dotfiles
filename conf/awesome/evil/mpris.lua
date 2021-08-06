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
		'ncspot',
		'mpd',
		'mopidy',
	}
	local status = "STOPPED"
	local artist
	local album
	local title
	local artUrl

	-- Compare player names for sorting.
	-- Source: https://github.com/BlingCorp/bling/blob/master/signal/playerctl/playerctl_lib.lua
	local function player_compare_name(name_a, name_b)
		local any_index = math.huge
		local a_match_index = nil
		local b_match_index = nil

		if name_a == name_b then
			return 0
		end

		for index, name in ipairs(supported_players) do
			if name == "%any" then
				any_index = (any_index == math.huge) and index or any_index
			elseif name == name_a then
				a_match_index = a_match_index or index
			elseif name == name_b then
				b_match_index = b_match_index or index
			end
		end

		if not a_match_index and not b_match_index then
			return 0
		elseif not a_match_index then
			return (b_match_index < any_index) and 1 or -1
		elseif not b_match_index then
			return (a_match_index < any_index) and -1 or 1
		elseif a_match_index == b_match_index then
			return 0
		else
			return (a_match_index < b_match_index) and -1 or 1
		end
	end

	-- Sorting function used by manager if a priority order is specified
	local function player_compare(a, b)
		local player_a = Playerctl.Player(a)
		local player_b = Playerctl.Player(b)
		return player_compare_name(player_a.player_name, player_b.player_name)
	end

	manager:set_sort_func(player_compare)

	function signal_update()
		awesome.emit_signal("evil::mpris::metadata", status, artist, title, album, artUrl)
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
			if status == "STOPPED" and player.playback_status == "PLAYING" then
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

	function manager:on_name_appeared(name)
		follow_player(name)
	end

	function manager:on_name_vanished()
		if #self.players == 0 then
			status = "STOPPED"
			artist = nil
			album = nil
			artUrl = nil
			title = nil
			signal_update()
		end
	end

	awesome.connect_signal("evil::mpris::change", function(action)
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
