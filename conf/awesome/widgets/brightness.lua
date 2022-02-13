local awful = require("awful")
local Block = require("widgets.block")
local rtrim = require("utils.rtrim")

return Block({
	timeout = 30,
	buttons = {
		[awful.button.names.LEFT] = function()
			awful.spawn("xbacklight -set 50", false)
		end,
		[awful.button.names.SCROLL_UP] = function()
			awful.spawn("xbacklight -dec 5", false)
		end,
		[awful.button.names.SCROLL_DOWN] = function()
			awful.spawn("xbacklight -inc 5", false)
		end,
	},
	cb = function(update)
		awful.spawn.easy_async_with_shell("xbacklight 2> /dev/null | cut -d . -f 1", function(output)
			if #output > 0 then
				update(" " .. rtrim(output) .. "%")
			end
		end)
	end
})
