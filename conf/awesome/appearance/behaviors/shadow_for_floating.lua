-- luacheck: globals client
local awful = require("awful")
local is_laptop = require("utils.is_laptop")
local function set_shadow(c)
	if c.floating and not c.maximized and not c.fullscreen then
		awful.spawn("xprop -id " .. c.window .. " -f _COMPTON_SHADOW 32c -set _COMPTON_SHADOW 1", false)
	else
		awful.spawn("xprop -id " .. c.window .. " -f _COMPTON_SHADOW 32c -set _COMPTON_SHADOW 0", false)
	end
end

if not is_laptop then
	client.connect_signal("manage", set_shadow)
	client.connect_signal("property::floating", set_shadow)
	client.connect_signal("request::geometry", set_shadow)
end
