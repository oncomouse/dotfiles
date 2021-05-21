-- luacheck: globals awesome
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")
local recolor_icons = require("widgets.utils.recolor-icons")

local brightness_widget = {}
awesome.connect_signal("evil::brightness::status", function(brght)
	if brightness_widget.widget ~= nil then
		brightness_widget.widget:set_brightness(brght)
	end
end)

local function create()
	local levels = {
		"high",
		"medium",
		"low",
	}
	local template_func = function(brght) return "status/scalable/display-brightness-" .. brght .. "-symbolic.svg" end
	local icons = recolor_icons(levels, template_func)
	local tt = {}
	brightness_widget.widget = wibox.widget{
		{
			image = levels["high"],
			forced_height = beautiful.icon_size,
			forced_width = beautiful.icon_size,
			widget = wibox.widget.imagebox,
			id = "image",
		},
		widget = wibox.container.place,
		set_brightness = function(self, brght)
			tt:set_text("Brightness: " .. tostring(brght))
			local icon = "low"
			if brght >= 75 then
				icon = "high"
			elseif brght >= 50 then
				icon = "medium"
			end
			self:get_children_by_id("image")[1]:set_image(icons[icon])
		end
	}

	local function change_brightness(command)
		awesome.emit_signal("evil::brightness::change", command)
	end
	function brightness_widget:up()
		change_brightness("up")
	end
	function brightness_widget:down()
		change_brightness("down")
	end
	function brightness_widget:default()
		change_brightness()
	end

	tt = awful.tooltip {
		objects = { brightness_widget.widget },
		mode = 'outside',
		preferred_positions = {'bottom'},
	}

	brightness_widget.widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			change_brightness()
		elseif button == 4 then
			change_brightness("up")
		elseif button == 5 then
			change_brightness("down")
		end
	end)

	return brightness_widget.widget
end

return setmetatable(brightness_widget, { __call = function(_, ...) return create(...) end })


