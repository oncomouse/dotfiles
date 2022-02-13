-- luacheck: globals awesome
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local Volume = {}

function Volume:new()
	return setmetatable({}, { __index = self }):init()
end
-- 1) dwm-volume.sh mute ;;
-- 4) dwm-volume.sh up ;;
-- 5) dwm-volume.sh down ;;

function Volume:init()
	self.widget = wibox.widget({
		{
			id = "output",
			markup = "",
			widget = wibox.widget.textbox,
		},
		layout = wibox.layout.fixed.horizontal,
		update_volume = function(volume, muted)
			local output
			if muted then
				output = "婢" .. " x"
			else
				output = "墳 " .. tostring(volume) .. "%"
			end
			self.widget.output.markup = output
		end,
	})
	self.widget.buttons = awful.util.table.join(
		awful.button({}, 1, require("widgets.keypress")("liskin-media mute", "volume")),
		awful.button({}, 4, require("widgets.keypress")("liskin-media volume up", "volume")),
		awful.button({}, 5, require("widgets.keypress")("liskin-media volume down", "volume"))
	)
	self.request_update = function()
		awesome.emit_signal("dotfiles::volume::request")
	end
	self.timer = gears.timer({
		timeout = 30,
		call_now = true,
		autostart = true,
		callback = self.request_update,
	})
	awesome.connect_signal("dotfiles::volume", function(volume, muted)
		self.widget.update_volume(volume, muted)
	end)
	self.request_update()
	return self
end

return setmetatable(Volume, { __call = Volume.new })
