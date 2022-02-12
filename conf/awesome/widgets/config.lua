-- Format of each block is { <shell command>, <update interval>, <signal name> }
local widgets = require("utils.is_laptop")
		and {
			{ "dwmblocks-volume.sh", 30, "volume" },
			{ "dwmblocks-brightness.sh", 30, "brightness" },
			{ "dwmblocks-battery.sh", 30, "battery" },
			{ "dwmblocks-date.sh", 5, "date" },
		}
	or {
		{ "dwmblocks-volume.sh", 30, "volume" },
		{ nil, 0, "heartbeat" },
		{ nil, 0, "mpris" },
		{ "dwmblocks-weather.sh", 600, "weather" },
		{ "dwmblocks-date.sh", 5, "date" },
	}

return widgets
