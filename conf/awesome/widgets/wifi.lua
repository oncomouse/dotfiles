-- luacheck: globals awesome
local wibox = require("wibox")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
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
		{
			image = strength == "" and nil or icons[strength],
			forced_height = beautiful.icon_size,
			forced_width = beautiful.icon_size,
			widget = wibox.widget.imagebox,
			id = "image",
		},
		widget = wibox.container.place,
		image = strength == "" and nil or icons[strength],
		set_strength = function(self, str)
			self:get_children_by_id("image")[1]:set_image(icons[str])
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
		tt.markup = "<b>SSID</b>: " .. ssid .. "\n<b>Strength</b>: " .. strength
	end)
	wifi_widget.widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			rofi()
		end
	end)

	return wifi_widget.widget
end

return setmetatable(wifi_widget, { __call = function(_, ...) return create(...) end })
