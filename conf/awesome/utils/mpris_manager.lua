-- luacheck: globals awesome
local beautiful = require("beautiful")
local gears = require("gears")
local lgi = require("lgi")

local Playerctl = lgi.Playerctl
beautiful.mpris_players = {
	"mpd",
	"ncspot",
	"mpv",
}
local manager = {}
manager.players = {}
manager.most_recent_player = nil
local gobject = lgi.Playerctl.PlayerManager()
function gobject:on_name_appeared(name)
	manager:follow_player(name)
end

function gobject:on_player_vanished(player)
	manager:unfollow_player(player.props.player_name)
end
gobject.on_name_appeared:connect("name-appeared")
gobject.on_player_vanished:connect("player-vanished")

function manager:follow_player(name)
	if gears.table.hasitem(beautiful.mpris_players, gears.string.split(name.name, ".")[1]) then
		local player = Playerctl.Player.new_from_name(name)
		player.on_metadata = function(p)
			manager.update(p)
		end
		gobject:manage_player(player)
		self.players[gears.string.split(name.name, ".")[1]] = player
		if self.most_recent_player == nil then
			self.most_recent_player = gears.string.split(name.name, ".")[1]
			manager.update(player)
		end
	end
end

function manager:unfollow_player(name)
	name = gears.string.split(name.name, ".")[1]
	if gears.table.hasitem(self.players, name) then
		self.players[name] = nil
		if self.most_recent_player == name then
			awesome.emit_signal("widget::mpris::update", nil)
		end
	end
end

function manager.update(player)
	awesome.emit_signal("widget::mpris::update", player.playback_status, {
		artist = player:get_artist(),
		album = player:get_album(),
		title = player:get_title(),
	})
end

awesome.connect_signal("widget::mpris::create_widget", function()
	for _, name in pairs(Playerctl.list_players()) do
		manager:follow_player(name)
	end
end)

-- Signals:
awesome.connect_signal("widget::mpris::action", function(action)
	local player
	if manager.most_recent_player ~= nil then
		player = manager.players[manager.most_recent_player]
	elseif #manager.players > 0 then
		player = manager.players[1]
	end
	if player ~= nil then
		if action == "play_pause" then
			player:play_pause()
		elseif action == "stop" then
			player:stop()
		elseif action == "next" then
			player:next()
		elseif action == "previous" then
			player:previous()
		end
		manager.update(player)
	end
end)

awesome.connect_signal("widget::mpris::keypress", function(action)
	if manager.most_recent_player ~= nil then
		awesome.emit_signal("widget::mpris::action", action)
	end
end)
