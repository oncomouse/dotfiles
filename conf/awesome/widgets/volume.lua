-- luacheck: globals awesome
local wibox = require("wibox")

local volume_widget = {}

local current_volume = ""
awesome.connect_signal("evil::volume::update", function(volume)
	current_volume = (not volume:find("x")) and string.gsub(volume, "^%s*(.-)%s*$", "%1") .. "%" or volume
	if volume_widget.widget ~= nil then
		volume_widget.widget:set_value(current_volume)
	end
end)

local function create()
	volume_widget.widget = wibox.widget{
		widget = wibox.widget.textbox,
		text = current_volume,
		set_value = function(self, volume)
			self:set_text(" 墳 " .. volume)
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
