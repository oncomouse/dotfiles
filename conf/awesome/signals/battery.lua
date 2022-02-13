-- luacheck: globals awesome
-- Source: https://github.com/elenapan/dotfiles/blob/master/config/awesome/evil/battery.lua
-- Provides:
-- dotfiles::battery::level
--      percentage (integer)
-- dotfiles::battery::charger
--      plugged (boolean)
-- Requires: acpid

local awful = require("awful")
local gears = require("gears")

local update_interval = 30

local level = nil
local charging = false

-- Subscribe to power supply status changes with acpi_listen
local charger_script = [[
	sh -c '
	acpi_listen | grep --line-buffered ac_adapter
	'
]]

gears.timer({
	timeout = update_interval,
	autostart = true,
	call_now = true,
	callback = function()
		awful.spawn.easy_async_with_shell("acpi -b | head -n  1", function(stdout)
			level = stdout:match("%d+%%"):gsub("%%", "")
			level = tonumber(level)
			awesome.emit_signal("dotfiles::battery::level", level)
		end)
	end
})

-- First get charger file path
awful.spawn.easy_async_with_shell(
	'sh -c \'out="$(find /sys/class/power_supply/*/online)" && (echo "$out" | head -1) || false\' ',
	function(charger_file, _, _, exit_code)
		-- No charger file found
		if not (exit_code == 0) then
			return
		end
		-- Then initialize function that emits charger info
		local emit_charger_info = function()
			awful.spawn.easy_async_with_shell("cat " .. charger_file, function(out)
				charging = tonumber(out) == 1
				awesome.emit_signal("dotfiles::battery::charger", charging)
			end)
		end

		-- Run once to initialize widgets
		emit_charger_info()

		-- Kill old acpi_listen process
		awful.spawn.easy_async_with_shell(
			"ps x | grep \"acpi_listen\" | grep -v grep | awk '{print $1}' | xargs kill",
			function()
				-- Update charger status with each line printed
				awful.spawn.with_line_callback(charger_script, {
					stdout = function(_)
						emit_charger_info()
					end,
				})
			end
		)
	end
)

awesome.connect_signal("dotfiles::battery::request", function()
	awesome.emit_signal("dotfiles::battery::status", level, charging)
end)
