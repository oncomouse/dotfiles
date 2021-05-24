-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")

local function init()
	local function run_script(argument)

		argument = argument or ""
		awful.spawn.easy_async_with_shell(
			"~/dotfiles/scripts/volume.sh " .. argument,
			function(stdout)
				awesome.emit_signal("evil::volume::update", stdout)
			end
		)
	end
	awesome.connect_signal("evil::volume::change", function(command)
		local argument = ""
		if command == "mute" then
			argument = "mute"
		elseif command == "up" then
			argument = "up 5"
		elseif command == "down" then
			argument = "down 5"
		end
		run_script(argument)
	end)
	gears.timer{
		timeout = 5,
		call_now = true,
		autostart = true,
		callback = function()
			run_script()
		end,
	}
end
return init
