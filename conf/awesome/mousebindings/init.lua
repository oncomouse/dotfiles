-- luacheck: globals client
local beautiful = require("beautiful")
local awful = require("awful")
require("mousebindings.config")
-- Client mouse behavior
client.connect_signal("mouse::enter", function(c) -- Sloppy focus
	if beautiful.sloppy_focus then
		c:emit_signal("request::activate", "mouse_enter", { raise = false })
	end
end)
client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings(beautiful.client_mousebuttons)
end)
