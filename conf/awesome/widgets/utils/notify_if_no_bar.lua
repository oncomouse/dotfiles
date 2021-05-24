-- luacheck: globals screen client
local naughty = require("naughty")


function notify_if_no_bar(opts)
	if not client.focus.screen.mywibox.visible then
		naughty.notify(opts)
	end
end

return notify_if_no_bar
