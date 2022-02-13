-- luacheck: globals awesome
-- Source: https://github.com/elenapan/dotfiles/blob/master/config/awesome/evil/brightness.lua
-- Provides:
-- dotfiles::brightness
--      percentage (integer)
local awful = require("awful")

-- Subscribe to backlight changes
-- Requires inotify-tools
local brightness_subscribe_script = [[
   bash -c "
   while (inotifywait -e modify /sys/class/backlight/?*/brightness -qq) do echo; done
"]]

local brightness_script = [[
   sh -c "
   xbacklight
"]]

local brightness = 0

local emit_brightness_info = function()
	awful.spawn.with_line_callback(brightness_script, {
		stdout = function(line)
			brightness = math.floor(tonumber(line))
			awesome.emit_signal("dotfiles::brightness::update", brightness)
		end,
	})
end

awesome.connect_signal("dotfiles::brightness::request", emit_brightness_info)

awesome.connect_signal("dotfiles::brightness::action", function(action)
	if action == "default" then
		awful.spawn("xbacklight -set 50", false)
	elseif action == "down" then
		awful.spawn("xbacklight -dec 5", false)
	elseif action == "up" then
		awful.spawn("xbacklight -inc 5", false)
	end
end)

-- Kill old inotifywait process
awful.spawn.easy_async_with_shell(
	"ps x | grep \"inotifywait -e modify /sys/class/backlight\" | grep -v grep | awk '{print $1}' | xargs kill",
	function()
		-- Update brightness status with each line printed
		awful.spawn.with_line_callback(brightness_subscribe_script, {
			stdout = function(_)
				emit_brightness_info()
			end,
		})
	end
)
