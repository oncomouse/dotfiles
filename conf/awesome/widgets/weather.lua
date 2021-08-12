local block = require("utils.block")
local weather_widget = {}

local function create()
	return block([===[/bin/sh -c '
	function full_weather {
		xdg-open "https://wttr.in"
		echo "require(\"awful\").client.urgent.jumpto()" | awesome-client
	}
	[[ "$BUTTON" = 1 ]] && full_weather
	curl -s "https://wttr.in/?format=1" | sed -e "s/\s\+//g" -e "s/+//g"
	']===], 20)
end
return setmetatable(weather_widget, { __call = function(_, ...)
	return create(...)
end })
