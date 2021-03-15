local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local function set_volume(widget, stdout)
	volume = stdout
	if not stdout:find("x") then
		volume = string.gsub(stdout, "^%s*(.-)%s*$", "%1") .. "%"
	end
	widget:set_text(" 墳 " .. volume)
end
local function run_script(widget, argument)
	argument = argument or ""
	awful.spawn.easy_async_with_shell(
		string.format("~/dotfiles/scripts/volume.sh %s", argument),
		function(stdout)
			set_volume(widget, stdout)
		end
	)
end
local function change_volume(volume_widget, change)
	local argument = ""
	if change == "mute" then
		argument = "mute"
	elseif change == "up" then
		argument = "up 5%"
	elseif change == "down" then
		argument = "down 5%"
	end
	run_script(volume_widget, argument)
end

local volume_widget = wibox.widget{
	widget = wibox.widget.textbox,
	text = "",
	mute = function(self)
		change_volume(self, "mute")
	end,
	up = function(self)
		change_volume(self, "up")
	end,
	down = function(self)
		change_volume(self, "down")
	end,
}
gears.timer{
	timeout = 5,
	call_now = true,
	autostart = true,
	callback = function()
		run_script(volume_widget)
	end,
}

volume_widget:connect_signal("button::press", function()
	change_volume(volume_widget, "mute")
end)

return volume_widget
