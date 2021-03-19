-- luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")

-- List of apps that should block screensaver if they are playing:
local apps = {
	"zoom",
	"vlc",
	"firefox"
}
-- How frequently to check:
local timeout = 120
-- What command to run to stop screensaver:
local delay_command = "xscreensaver-command -deactivate"
local stop_now = false

function check()
	if stop_now then
		awful.spawn(delay_command)
		return
	end
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

gears.timer{
	timeout = timeout,
	autostart = true,
	call_now = false,
	callback = check
}

awesome.connect_signal("heartbeat::stop_now", function()
	stop_now = not stop_now
end)

