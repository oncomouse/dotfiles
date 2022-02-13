-- Format of each block is { <shell command>, <update interval>, <signal name> }
local widgets = require("utils.is_laptop")
		and {
			{ nil, 0, "volume" },
			{ "dwmblocks-brightness.sh", 30, "brightness" },
			{ "dwmblocks-battery.sh", 30, "battery" },
			{ nil, 0, "date" },
		}
	or {
		{ nil, 0, "volume" },
		{ nil, 0, "heartbeat" },
		{ nil, 0, "mpris" },
		{ "dwmblocks-weather.sh", 600, "weather" },
		{ nil, 0, "date" },
	}

return widgets
