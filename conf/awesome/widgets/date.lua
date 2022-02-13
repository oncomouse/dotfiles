-- luacheck: globals awesome
local wibox = require("wibox")

local Date = {}

function Date:new()
	return setmetatable({}, { __index = self }):init()
end

function Date:init()
	self.widget = wibox.widget({
		{
			format = "%a %m/%d %-I:%M %p ",
			widget = wibox.widget.textclock
		},
		layout = wibox.layout.fixed.horizontal,
	})
	return self
end

return setmetatable(Date, { __call = Date.new })
