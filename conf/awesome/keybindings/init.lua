-- luacheck: globals client
local awful = require("awful")
local beautiful = require("beautiful")
require("keybindings.config")
-- Keybindings:
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings(beautiful.client_keybindings)
end)
awful.keyboard.append_global_keybindings(beautiful.global_keybindings)
