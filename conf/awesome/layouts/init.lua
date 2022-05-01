local awful = require("awful")
local beautiful = require("beautiful")
require("layouts.config")
-- Attach default layouts
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts(beautiful.default_layouts)
end)
