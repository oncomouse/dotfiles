-- luacheck: globals awesome
local awful = require("awful")
local rtrim = require("utils.rtrim")
local Block = require("widgets.block")

return Block({
	name = "weather",
	buttons = {
		[awful.button.names.LEFT] = function() awful.spawn('xdg-open "https://wttr.in"', false) end,
	},
	callback = function(update)
		awful.spawn.easy_async_with_shell(
			'curl -s "https://wttr.in/?format=1" | sed -e "s/ +//" -e "s/Unknown.*\\$//"',
			function(stdout)
				update(rtrim(stdout))
			end
		)
	end,
	timeout = 600,
})
