-- luacheck: globals awesome
local wibox = require("wibox")
local beautiful = require("beautiful")
local trim = require("utils.trim")
local recolor_icons = require("widgets.utils.recolor-icons")
local awful = require("awful")

local volume_widget = {}

local current_volume = ""
awesome.connect_signal("evil::volume::update", function(volume)
	current_volume = (not volume:find("x")) and string.gsub(volume, "^%s*(.-)%s*$", "%1") .. "%" or volume
	if volume_widget.widget ~= nil then
		volume_widget.widget:set_value(current_volume)
	end
end)

local function create()
	local icons = recolor_icons({
		"muted",
		"low",
		"medium",
		"high",
	}, function(x) return "status/scalable/audio-volume-" .. x .. "-symbolic.svg" end)
	local tt = {}
	volume_widget.widget = wibox.widget{
		{
			image = icons["muted"],
			forced_height = beautiful.icon_size,
			forced_width = beautiful.icon_size,
			widget = wibox.widget.imagebox,
			id = "image",
		},
		widget = wibox.container.place,
		set_value = function(self, volume)
			if volume == "x" then
				self:set_image(icons["muted"])
				tt:set_text("muted")
			else
				tt:set_text(volume)
				volume = string.gsub(trim(volume), "%%", "")
				local v = tonumber(volume)
				local icon = "low"
				if v >= 75 then
					icon = "high"
				elseif v >= 50 then
					icon = "medium"
				end
				self:get_children_by_id("image")[1]:set_image(icons[icon])
			end
		end
	}
	local function change_volume(command)
		awesome.emit_signal("evil::volume::change", command)
	end
	function volume_widget:mute()
		change_volume("mute")
	end
	function volume_widget:up()
		change_volume("up")
	end
	function volume_widget:down()
		change_volume("down")
	end

	tt = awful.tooltip {
		objects = { volume_widget.widget },
		mode = 'outside',
		preferred_positions = {'bottom'},
	}

	volume_widget.widget:connect_signal("button::press", function(_, _, _, button)
		if button == 1 then
			change_volume("mute")
		elseif button == 4 then
			change_volume("up")
		elseif button == 5 then
			change_volume("down")
		end
	end)

	return volume_widget.widget
end

return setmetatable(volume_widget, { __call = function(_, ...)
	return create(...)
end })
