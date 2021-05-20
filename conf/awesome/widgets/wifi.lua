-- luacheck: globals awesome
-- local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()
local rofi = require("utils.rofi").networkmanager

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
	local icons = {
		excellent = gears.color.recolor_image("/usr/share/icons/Adwaita/scalable/status/network-wireless-signal-excellent-symbolic.svg", xrdb.color7),
		good = gears.color.recolor_image("/usr/share/icons/Adwaita/scalable/status/network-wireless-signal-good-symbolic.svg", xrdb.color7),
		ok = gears.color.recolor_image("/usr/share/icons/Adwaita/scalable/status/network-wireless-signal-ok-symbolic.svg", xrdb.color7),
		weak = gears.color.recolor_image("/usr/share/icons/Adwaita/scalable/status/network-wireless-signal-weak-symbolic.svg", xrdb.color7),
	}
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
