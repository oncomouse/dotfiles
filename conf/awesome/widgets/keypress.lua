-- luacheck: globals awesome
local awful = require("awful")
-- Handler for key presses
local function media_key_press(cmd, signal)
	return function()
		awful.spawn.easy_async_with_shell(cmd, function()
			awesome.emit_signal("dotfiles::update", signal)
		end)
	end
end

return media_key_press
