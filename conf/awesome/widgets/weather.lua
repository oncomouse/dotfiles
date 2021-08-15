local awful = require("awful")
local weather_widget = {}

local function create()
	local widget = awful.widget.watch([[/bin/sh -c 'curl -s "https://wttr.in/?format=1" | sed -e "s/\s\+//g" -e "s/+//g"']], 20, function(widget, stdout)
		widget:set_markup(stdout)
		widget:add_button(awful.button({
			modifiers = {},
			button = awful.button.LEFT,
			on_press = function()
				awful.spawn.easy_async("xdg-open 'https://wttr.in'", function()
					awful.client.urgent.jumpto()
				end)
			end
		}))
	end)
	return widget
end
return setmetatable(weather_widget, { __call = function(_, ...)
	return create(...)
end })
