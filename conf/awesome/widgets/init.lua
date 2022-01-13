-- luacheck: globals awesome
local beauitful = require("beautiful")
beauitful.mpris_players = {
	"mpd",
	"ncspot",
	"mpv",
}
local most_recent_player = nil
local players = {}

awesome.connect_signal("widget::mpris::manage", function(widget)
	players[widget.name] = widget
end)
awesome.connect_signal("widget::mpris::unmanage", function(widget)
	players[widget.name] = nil
	if most_recent_player == widget.name then
		most_recent_player = nil
	end
end)
awesome.connect_signal("widget::mpris::update", function(name)
	most_recent_player = name
end)
awesome.connect_signal("widget::mpris::keypress", function(action)
	if most_recent_player ~= nil then
		players[most_recent_player].widget:emit_signal("widget::mpris::action", action)
	end
end)

return require("widgets.make")(require("widgets.config"))
