-- luacheck: globals awesome
-- Adapted from: https://raw.githubusercontent.com/macunha1/awesomewm-media-player-widget/master/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local lgi = require("lgi")

local Playerctl = lgi.Playerctl
local wibox = require("wibox")

local MediaPlayer = {}

function MediaPlayer:new(args)
	return setmetatable({}, { __index = self }):init(args)
end

function MediaPlayer:init(args)
	self.font = args.font or beautiful.font
	self.icons = args.icons or {
		PLAYING = "契 ",
		PAUSED = " ",
		STOPPED = "栗",
	}
	self.name = args.name or 'mpd'
	self.status = nil
	self.player = nil

	self.autohide = args.autohide == nil
	self.widget = wibox.widget({
		{
			id = "icon",
			widget = wibox.widget.textbox,
		},
		{
			id = "current_song",
			widget = wibox.widget.textbox,
			font = self.font,
		},
		layout = wibox.layout.fixed.horizontal,
		set_status = function(widget, icon)
			widget.icon.markup = icon
		end,
		set_text = function(widget, text)
			widget.current_song.markup = text
		end,
		set_visible = function(widget, show_or_hide)
			widget.icon.visible = show_or_hide
			widget.current_song = show_or_hide
		end,
	})

	self:signal()

	return self
end

function MediaPlayer:escape_xml(str)
	str = string.gsub(str, "&", "&amp;")
	str = string.gsub(str, "<", "&lt;")
	str = string.gsub(str, ">", "&gt;")
	str = string.gsub(str, "'", "&apos;")
	str = string.gsub(str, '"', "&quot;")

	return str
end

function MediaPlayer:update_widget_text(player)
	awesome.emit_signal("widget::mpris::update", self.name)
	if player.playback_status == nil or player.playback_status == "STOPPED" then
		self:hide_widget()
		return
	end
	local artist = player:get_artist()
	local title = player:get_title()
	local output = string.format("%s - %s", artist, title)
	self.widget:set_text(self:escape_xml(output))
	self.widget:set_status(self.icons[player.playback_status])
	self.widget:set_visible(true)
end

function MediaPlayer:hide_widget()
	self.widget:set_text("Offline")
	self.widget:set_status(self.icons.STOPPED)
	self.widget:set_visible(not self.autohide)
end

function MediaPlayer:signal()
	-- Connect Playerctl:
	local function follow_player(name)
		if not self.player and self.name == gears.string.split(name.name, ".")[1] then
			self.player = Playerctl.Player.new_from_name(name)
			self.player.on_metadata = function(player)
				self:update_widget_text(player)
			end
			self:update_widget_text(self.player)
			awesome.emit_signal("widget::mpris::manage", self)
		end
	end

	local function unfollow_player(name)
		if self.player and self.name == gears.string.split(name.name, ".")[1] then
			self.player = nil
			self.player.on_metadata = function() end
			self:hide_widget()
			awesome.emit_signal("widget::mpris::unmanage", self)
		end
	end

	self.manager = lgi.Playerctl.PlayerManager()
	self.manager.on_name_appeared:connect("name-appeared")
	function self.manager:on_name_appeared(name)
		follow_player(name)
	end

	function self.manager:on_player_vanished(player)
		unfollow_player(player.props.player_name)
	end

	for _, name in pairs(Playerctl.list_players()) do
		follow_player(name)
	end

	-- Collection Action Signals:
	self.widget:connect_signal("widget::mpris::action", function(_, action)
		if action == "play_pause" then
			self.player:play_pause()
		elseif action == "stop" then
			self.player:stop()
		elseif action == "next" then
			self.player:next()
		elseif action == "previous" then
			self.player:previous()
		end
	end)

	-- Connect Buttons
	self.widget:buttons(awful.util.table.join(
		awful.button(
			{},
			1, -- button 1: left click  - play/pause
			function()
				self.widget:emit_signal("widget::mpris::action", "play_pause")
			end
		),
		awful.button(
			{},
			3, -- button 4: scroll up   - next song
			function()
				self.widget:emit_signal("widget::mpris::action", "next")
			end
		),
		awful.button(
			{},
			2, -- button 5: scroll down - previous song
			function()
				self.widget:emit_signal("widget::mpris::action", "previous")
			end
		)
	))
end

return setmetatable(MediaPlayer, { __call = MediaPlayer.new })
