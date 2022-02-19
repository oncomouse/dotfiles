local awful = require("awful")
local beautiful = require("beautiful")
require("keybindings.config")
-- Client Keybindings:
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings(beautiful.client_keybindings)
end)
-- Global Keybingds:
awful.keyboard.append_global_keybindings(beautiful.global_keybindings)
