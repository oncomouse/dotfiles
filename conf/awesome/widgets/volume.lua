-- luacheck: globals awesome
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local trim = require("utils.trim")
local recolor_icons = require("widgets.utils.recolor-icons")
local notify_if_no_bar = require("widgets.utils.notify_if_no_bar")

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
	local change = false
	volume_widget.widget = wibox.widget{
		{
			{
				{
					image = icons["muted"],
					forced_height = beautiful.icon_size,
					forced_width = beautiful.icon_size,
					widget = wibox.widget.imagebox,
					id = "image",
				},
				widget = wibox.container.place,
			},
			{
				widget = wibox.widget.textbox,
				markup = "",
				id = "display"
			},
			layout = wibox.layout.fixed.horizontal,
		},
		widget = wibox.container.margin,
		set_value = function(self, volume)
			volume = trim(volume)
			if volume == nil or volume == "x" then
				self:get_children_by_id("display")[1]:set_markup("")
				self:get_children_by_id("image")[1]:set_image(icons["muted"])
				if change then
					notify_if_no_bar({icon=icons["muted"], text="Volume is muted"})
					change = false
				end
			else
				self:get_children_by_id("display")[1]:set_markup(volume)
				local v = string.gsub(volume, "%%", "")
				v = tonumber(v)
				local icon = "low"
				if v >= 75 then
					icon = "high"
				elseif v >= 50 then
					icon = "medium"
				end
				if change then
					notify_if_no_bar({icon=icons[icon], text="Volume is " .. volume})
					change = false
				end
				self:get_children_by_id("image")[1]:set_image(icons[icon])
			end
		end
	}
	local function change_volume(command)
		change = true
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
