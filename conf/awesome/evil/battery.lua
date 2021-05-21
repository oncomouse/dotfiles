-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local trim = require("utils.trim")

local function init()
	local charge = 100
	local charging = true
	local function check_charge()
		awful.spawn.easy_async("acpi -b", function(stdout)
			local parts = gears.string.split(trim(stdout), ",")
			charge = tonumber(string.sub(parts[2], string.find(parts[2], "%d+")))
			charging = string.find(parts[1], "Charging") ~= nil
			awesome.emit_signal("evil::battery::status", charge, charging)
		end)
	end
	gears.timer{
		timeout = 25,
		call_now = true,
		autostart = true,
		callback = check_charge,
	}
end

return init

