-- Format of each block is { <shell command>, <update interval>, <signal name> }
local widgets = require("utils.is_laptop") and {
	"volume",
	"brightness",
	"battery",
	"date",
} or {
	"volume",
	"heartbeat",
	"mpris",
	"weather",
	"date",
}

return widgets
