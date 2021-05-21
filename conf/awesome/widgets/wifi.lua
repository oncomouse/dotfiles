-- luacheck: globals awesome
-- local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()
local rofi = require("utils.rofi").networkmanager
local recolor_icons = require("widgets.utils.recolor-icons")

local wifi_widget = {}
local strength = ""
local ssid = ""
awesome.connect_signal("evil::wifi::strength", function(str)
	strength = str
	if wifi_widget.widget ~= nil then
		wifi_widget.widget:set_strength(str)
	end
end)
awesome.connect_signal("evil::wifi::ssid", function(str)
	ssid = str
end)

local function create()
	local strengths = {
		"excellent",
		"good",
		"ok",
		"weak",
		"none"
	}
	local template_func = function(st) return "network-wireless-signal-" .. st .. "-symbolic.svg" end
	local icons = recolor_icons(strengths, template_func)
	wifi_widget.widget = wibox.widget{
		image = strength == "" and nil or icons[strength],
		widget = wibox.widget.imagebox,
		set_strength = function(self, str)
			self:set_image(icons[str])
		end
	}

	wifi_widget.click = function()
		rofi()
	end

	local tt = awful.tooltip {
		objects = { wifi_widget.widget },
		mode = 'outside',
		preferred_positions = {'bottom'},
	}
	wifi_widget.widget:connect_signal('mouse::enter', function()
		tt.text = ssid
	end)
	wifi_widget.widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			rofi()
		end
	end)

	return wifi_widget.widget
end

return setmetatable(wifi_widget, { __call = function(_, ...) return create(...) end })
