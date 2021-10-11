-- luacheck: globals client awesome
client.connect_signal("manage", function (c)
	if not awesome.startup then require("awful").client.setslave(c) end
end)

