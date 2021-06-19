local naughty = require("naughty")
local awful = require("awful")

function notify_if_no_bar(opts)
	if not awful.screen.focused().mywibox.visible then
		naughty.notify(opts)
	end
end

return notify_if_no_bar
