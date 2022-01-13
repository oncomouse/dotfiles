-- luacheck: globals awesome
-- Adapted from: https://raw.githubusercontent.com/macunha1/awesomewm-media-player-widget/master/init.lua
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local MediaPlayer = {}

function MediaPlayer:new(args)
	return setmetatable({}, { __index = self }):init(args)
end

function MediaPlayer:init(args)
	args = args or {}
	self.font = args.font or beautiful.font
	self.icons = args.icons or {
		PLAYING = "契 ",
		PAUSED = " ",
		STOPPED = "栗",
	}
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
			widget.current_song.visible = show_or_hide
		end,
	})

	self:signal()

	return self
end

function MediaPlayer.escape_xml(str)
	str = string.gsub(str, "&", "&amp;")
	str = string.gsub(str, "<", "&lt;")
	str = string.gsub(str, ">", "&gt;")
	str = string.gsub(str, "'", "&apos;")
	str = string.gsub(str, '"', "&quot;")

	return str
end

function MediaPlayer:update_widget_text(status, metadata)
	if status == nil or status == "STOPPED" then
		self:hide_widget()
		return
	end
	local artist = metadata.artist
	local title = metadata.title
	local output = string.format("%s - %s", artist, title)
	self.widget:set_text(self.escape_xml(output))
	self.widget:set_status(self.icons[status])
	self.widget:set_visible(true)
end

function MediaPlayer:hide_widget()
	self.widget:set_text("Offline")
	self.widget:set_status(self.icons.STOPPED)
	self.widget:set_visible(not self.autohide)
end

function MediaPlayer:signal()
	-- Connect Playerctl:
	awesome.connect_signal("widget::mpris::update", function(...) self:update_widget_text(...) end)
	awesome.emit_signal("widget::mpris::create_widget")

	-- Connect Buttons
	self.widget:buttons(awful.util.table.join(
		awful.button(
			{},
			1, -- button 1: left click  - play/pause
			function()
				awesome.emit_signal("widget::mpris::action", "play_pause")
			end
		),
		awful.button(
			{},
			3, -- button 4: scroll up   - next song
			function()
				awesome.emit_signal("widget::mpris::action", "next")
			end
		),
		awful.button(
			{},
			2, -- button 5: scroll down - previous song
			function()
				awesome.emit_signal("widget::mpris::action", "previous")
			end
		)
	))
end

return setmetatable(MediaPlayer, { __call = MediaPlayer.new })
