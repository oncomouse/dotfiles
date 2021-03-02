local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local function set_volume_text(widget, volume)
	widget:set_text(" 墳 " .. volume)
end
local function volume_change(volume_widget, change)
	local argument = ""
	if change == "mute" then
		argument = "mute"
	elseif change == "up" then
		argument = "up 5%"
	elseif change == "down" then
		argument = "down 5%"
	end
	awful.spawn.easy_async_with_shell(
		string.format("~/dotfiles/scripts/volume.sh %s", argument),
		function(stdout)
			if stdout:find("x") then
				set_volume_text(volume_widget, stdout)
			else
				set_volume_text(
					volume_widget,
					string.gsub(stdout, "^%s*(.-)%s*$", "%1") .. "%"
				)
			end
		end
	)
end

local volume_widget = wibox.widget{
	widget = wibox.widget.textbox,
	text = "",
	enable = function(self)
		volume_change(self, "enable")
	end,
	mute = function(self)
		volume_change(self, "mute")
	end,
	up = function(self)
		volume_change(self, "up")
	end,
	down = function(self)
		volume_change(self, "down")
	end,
}
gears.timer{
	timeout = 5,
	call_now = true,
	autostart = true,
	callback = function()
		awful.spawn.easy_async_with_shell(
			"~/dotfiles/scripts/volume.sh",
			function(out)
				set_volume_text(volume_widget, out)
			end
		)
	end,
}

volume_widget:connect_signal("button::press", function()
	volume_widget:mute()
end)

return volume_widget
