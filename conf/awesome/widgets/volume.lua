local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local volume_widget = {}

local function create()
	local function set_volume(widget, stdout)
		local volume = (not stdout:find("x")) and string.gsub(stdout, "^%s*(.-)%s*$", "%1") .. "%" or stdout
		widget:set_text(" 墳 " .. volume)
	end
	local function run_script(widget, argument)
		argument = argument or ""
		awful.spawn.easy_async_with_shell(
			"~/dotfiles/scripts/volume.sh " .. argument,
			function(stdout)
				set_volume(widget, stdout)
			end
		)
	end
	local function change_volume(widget, change)
		local argument = ""
		if change == "mute" then
			argument = "mute"
		elseif change == "up" then
			argument = "up 5"
		elseif change == "down" then
			argument = "down 5"
		end
		run_script(widget, argument)
	end

	volume_widget.widget = wibox.widget{
		widget = wibox.widget.textbox,
		text = "",
	}
	gears.timer{
		timeout = 5,
		call_now = true,
		autostart = true,
		callback = function()
			run_script(volume_widget.widget)
		end,
	}
	function volume_widget:mute()
		change_volume(volume_widget.widget.widget, "mute")
	end
	function volume_widget:up()
		change_volume(volume_widget.widget, "up")
	end
	function volume_widget:down()
		change_volume(volume_widget.widget, "down")
	end

	volume_widget.widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			change_volume(volume_widget.widget, "mute")
		elseif button == 4 then
			change_volume(volume_widget.widget, "up")
		elseif button == 5 then
			change_volume(volume_widget.widget, "down")
		end
	end)

	return volume_widget.widget
end

return setmetatable(volume_widget, { __call = function(_, ...)
	return create(...)
end })
