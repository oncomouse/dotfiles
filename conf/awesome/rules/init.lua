local ruled = require("ruled")
local beautiful = require("beautiful")
local awful = require("awful")
require("rules.config")
ruled.client.connect_signal("request::rules", function()
	for _, rule in ipairs(beautiful.rules) do
		ruled.client.append_rule(rule)
	end
end)

ruled.notification.connect_signal("request::rules", function()
	-- All notifications will match this rule.
	ruled.notification.append_rule({
		rule = {},
		properties = {
			screen = awful.screen.preferred,
			implicit_timeout = 5,
		},
	})
end)
