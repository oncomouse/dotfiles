-- luacheck: globals awesome
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")
local recolor_icons = require("widgets.utils.recolor-icons")

local battery_widget = {}
local charge = 100
local charging = true
awesome.connect_signal("evil::battery::status", function(chrg, crng)
	charge = chrg
	charging = crng
	if battery_widget.widget ~= nil then
		battery_widget.widget:set_battery(chrg)
	end
end)

local function create()
	local levels = {
		"full",
		"good",
		"low",
		"caution",
		"empty",
		"full-charging",
		"good-charging",
		"low-charging",
		"caution-charging",
		"empty-charging",
	}
	local template_func = function(chrg) return "status/scalable/battery-" .. chrg .. "-symbolic.svg" end
	local icons = recolor_icons(levels, template_func)
	local tt = {}
	battery_widget.widget = wibox.widget{
		{
			image = levels["high-charging"],
			forced_height = beautiful.icon_size,
			forced_width = beautiful.icon_size,
			widget = wibox.widget.imagebox,
			id = "image",
		},
		widget = wibox.container.place,
		set_battery = function(self, chrg)
			tt:set_text("Battery Status: " .. tostring(chrg) .. "%" .. (charging and ", Charging" or ""))
			icon = "empty"
			if chrg >= 15 and chrg < 40 then
				icon = "caution"
			elseif chrg >= 40 and charge < 60 then
				icon = "low"
			elseif chrg >= 60 and charge < 80 then
				icon = "good"
			elseif chrg >= 80 and charge < 100 then
				icon = "full"
			end
			if charging then
				icon = icon .. "-charging"
			end
			self:get_children_by_id("image")[1]:set_image(icons[icon])
		end
	}

	tt = awful.tooltip {
		objects = { battery_widget.widget },
		mode = 'outside',
		preferred_positions = {'bottom'},
	}

	return battery_widget.widget
end

return setmetatable(battery_widget, { __call = function(_, ...) return create(...) end })



