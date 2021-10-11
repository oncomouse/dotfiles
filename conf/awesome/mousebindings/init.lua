-- luacheck: globals client
local beautiful = require("beautiful")
local awful = require("awful")
require("mousebindings.config")
client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings(beautiful.client_mousebuttons)
end)
