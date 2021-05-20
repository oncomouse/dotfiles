-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local trim = require("utils.trim")

local function init()
	local function check_strength()
		awful.spawn.easy_async_with_shell("nmcli -t -f IN-USE,SIGNAL dev wifi list | grep \"^\\*\" | cut -d : -f 2", function(stdout)
			strength = tonumber(trim(stdout))
			if strength > 80 then
				awesome.emit_signal("evil::wifi::strength", "excellent")
			elseif strength > 50 then
				awesome.emit_signal("evil::wifi::strength", "good")
			elseif strength > 30 then
				awesome.emit_signal("evil::wifi::strength", "ok")
			else
				awesome.emit_signal("evil::wifi::strength", "weak")
			end
		end)
		awful.spawn.easy_async_with_shell("nmcli -t -f IN-USE,SSID dev wifi list | grep \"^\\*\" | cut -d : -f 2", function(stdout)
			awesome.emit_signal("evil::wifi::ssid", trim(stdout))
		end)
	end
	gears.timer{
		timeout = 60,
		call_now = true,
		autostart = true,
		callback = check_strength,
	}
end

return init
