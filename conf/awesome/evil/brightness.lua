-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local trim = require("utils.trim")

local change_amount = 5
local default_brightness = 40

local function init()
	local brightness = 0
	local function check_brightness()
		awful.spawn.easy_async("xbacklight", function(stdout)
			brightness = math.floor(tonumber(trim(stdout)))
			awesome.emit_signal("evil::brightness::status", brightness)
		end)
	end
	awesome.connect_signal("evil::brightness::change", function(direction)
		local command = "xbacklight -set " .. default_brightness
		if direction == "up" or direction == "down" then
			command = "xbacklight -" .. (direction == "up" and "inc" or "dec") .. " " .. change_amount
		end
		awful.spawn.easy_async(command, check_brightness)
	end)
	gears.timer{
		timeout = 60,
		call_now = true,
		autostart = true,
		callback = check_brightness,
	}
end

return init
