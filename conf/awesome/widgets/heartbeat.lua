-- luacheck: globals awesome
local awful = require("awful")
local wibox = require("wibox")

local status = {
	on = "",
	off = "﯈",
}

local Heartbeat = {}

function Heartbeat:new()
	return setmetatable({}, { __index = self }):init()
end

function Heartbeat:init()
	self.status = false
	self.widget = wibox.widget({
		{
			markup = status.off,
			id = "icon",
			widget = wibox.widget.textbox,
			forced_width = 20,
			align = "center"
		},
		layout = wibox.layout.fixed.horizontal,
	})
	self.set_status = function(on)
		if on == nil then
			on = not self.status
		end
		self.status = on
		self.widget.icon.markup = self.status and status.on or status.off
	end
	self.widget.buttons = {
		awful.button(
			{},
			1, -- button 1: left click  - play/pause
			function()
				awesome.emit_signal("dotfiles::heartbeat::toggle")
			end
		)
	}
	awesome.connect_signal("dotfiles::heartbeat::update", function(on)
		self.set_status(on)
	end)
	awesome.emit_signal("dotfiles::heartbeat::create")
	return self
end

return setmetatable(Heartbeat, { __call = Heartbeat.new })
