-- Adapted from: https://raw.githubusercontent.com/macunha1/awesomewm-media-player-widget/master/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local proxy = require("dbus_proxy")
local wibox = require("wibox")

local MediaPlayer = {}

function MediaPlayer:new(args)
	return setmetatable({}, { __index = self }):init(args)
end

function MediaPlayer:init(args)
	self.font = args.font or beautiful.font
	self.icons = args.icons or {
		play = "契 ",
		pause = " ",
		stop = "栗",
	}

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
		set_status = function(self, icon)
			self.icon.markup = icon
		end,
		set_text = function(self, text)
			self.current_song.markup = text
		end,
	})

	self.dbus = proxy.monitored.new({
		bus = proxy.Bus.SESSION,
		name = "org.mpris.MediaPlayer2." .. args.name or "spotify",
		path = "/org/mpris/MediaPlayer2",
		interface = "org.mpris.MediaPlayer2.Player",
	})

	-- Higher refresh_rate == less CPU requirements
	-- Lower refresh_rate == better Widget response time
	self:watch(args.refresh_rate or 3)
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

function MediaPlayer:update_widget_icon(output)
	if output == nil then return end
	output = string.gsub(output, "\n", "")
	self.widget:set_status((output == "Playing") and self.icons.play or self.icons.pause)
end

function MediaPlayer:update_widget_text(output)
	self.widget:set_text(self:escape_xml(output))
	self.widget:set_visible(true)
end

function MediaPlayer:hide_widget()
	self.widget:set_text("Offline")
	self.widget:set_status(self.icons.stop)
	self.widget:set_visible(not self.autohide)
end

function MediaPlayer:info()
	if not self.dbus.is_connected then
		return {}
	end

	local metadata = self.dbus:Get(self.dbus.interface, "Metadata")
	local status = self.dbus:Get(self.dbus.interface, "PlaybackStatus")

	local artists = metadata["xesam:artist"]
	if type(artists) == "table" then
		artists = table.concat(artists, ", ")
	end

	local info = {
		album = metadata["xesam:album"],
		title = metadata["xesam:title"],
		artists = artists,
		status = status,
	}

	return info
end

function MediaPlayer:watch(refresh_rate)
	local update_widget = function()
		local info = self:info()
		-- Status unavailable? Media Player isn't active, hide the widget
		if not info["status"] then
			self:hide_widget()
		else
			if not (info.artists == nil or info.title == nil) then
				-- Change the artist and title fields in case they aren't nil
				self:update_widget_icon(info["status"])
				self:update_widget_text(string.format("%s - %s", info.artists, info.title))
			end
		end
	end

	gears.timer({
		autostart = true,
		call_now = true,
		callback = update_widget,
		timeout = refresh_rate,
	})
end

function MediaPlayer:signal()
	local spawn_update = function()
		if self.widget.visible then -- Only respond to signals if visible (ie. playing or paused)
			local info = self:info()
			self:update_widget_icon(info["status"])
		end
	end

	-- Connect to my widget system:
	self.widget:connect_signal("widget::update", spawn_update)

	self.widget:buttons(awful.util.table.join(
		awful.button(
			{},
			1, -- button 1: left click  - play/pause
			function()
				self.dbus:PlayPause()
				spawn_update()
			end
		),
		awful.button(
			{},
			3, -- button 4: scroll up   - next song
			function()
				self.dbus:Next()
				spawn_update()
			end
		),
		awful.button(
			{},
			2, -- button 5: scroll down - previous song
			function()
				self.dbus:Previous()
				spawn_update()
			end
		)
	))
end

return setmetatable(MediaPlayer, { __call = MediaPlayer.new })
