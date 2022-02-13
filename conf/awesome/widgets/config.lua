-- Format of each block is { <shell command>, <update interval>, <signal name> }
local widgets = require("utils.is_laptop") and {
	"systray",
	"volume",
	"brightness",
	"battery",
	"date",
} or {
	"systray",
	"volume",
	"heartbeat",
	"mpris",
	"weather",
	"date",
}

return widgets
