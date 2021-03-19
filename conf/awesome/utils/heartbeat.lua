-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")

-- List of apps that should block screensaver if they are playing audio:
local init = function()
	local apps = beautiful.heartbeat_apps or {
		"zoom",
		"vlc",
		"firefox"
	}
	-- How frequently to check:
	local timeout = beautiful.heartbeat_timeout or 120
	-- What command to run to stop screensaver:
	local delay_command = beautiful.heartbeat_delay_command or "xscreensaver-command -deactivate"
	-- Only use forced_stop (disable app-based checking):
	local forced_only = beautiful.heartbeat_forced_only or false
	local forced_stop = false

	function check()
		if forced_stop then
			awful.spawn(delay_command)
			return
		end
		if not forced_only then
			awful.spawn.easy_async_with_shell("pactl list sink-inputs", function(out)
				local playing = false
				local stop = false
				for line in out:gmatch("[^\r\n]+") do
					local corked = string.match(line, "Corked: (%a*)")
					if corked == "no" then
						playing = true
					elseif corked == "yes" then
						playing = false
					end
					if not stop then
						local app = string.match(line, 'application.name.*"([^"]+)')
						if playing and app ~= nil then
							for _,needle in pairs(apps) do
								if string.match(app:upper(), needle:upper()) then
									stop = true
								end
							end
						end
					end
				end
				if stop then
					awful.spawn(delay_command)
				end
			end)
		end
	end

	gears.timer{
		timeout = timeout,
		autostart = true,
		call_now = false,
		callback = check
	}

	-- Use these three signals to build widgets and keyboard shortcuts that caffeinate or decaffeinate your system:
	-- Run awesome.emit_signal("heartbeat::toggle_forced_stop") to toggle forced stop
	awesome.connect_signal("heartbeat::toggle_forced_stop", function()
		forced_stop = not forced_stop
	end)
	-- Run awesome.emit_signal("heartbeat::start_forced_stop") to start forced stop
	awesome.connect_signal("heartbeat::start_forced_stop", function()
		forced_stop = true
	end)
	-- Run awesome.emit_signal("heartbeat::stop_forced_stop") to stop forced stop
	awesome.connect_signal("heartbeat::stop_forced_stop", function()
		forced_stop = false
	end)
end

return {
	init = init
}
