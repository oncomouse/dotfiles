local ruled = require("ruled")
local beautiful = require("beautiful")
require("rules.config")
ruled.client.connect_signal("request::rules", function()
	for _, rule in ipairs(beautiful.rules) do
		ruled.client.append_rule(rule)
	end
end)
