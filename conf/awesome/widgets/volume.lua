-- luacheck: globals awesome
local awful = require("awful")
local block = require("utils.block")

local volume_widget = {}

local function create()
	local widget, signal, trigger = block("~/dotfiles/scripts/volume.sh")

	awesome.connect_signal("dotfiles::volume", function(direction)
		trigger(direction, function()
			awesome.emit_signal(signal)
		end)
	end)

	function volume_widget:up()
		awesome.emit_signal("dotfiles::volume", "up")
	end

	function volume_widget:down()
		awesome.emit_signal("dotfiles::volume", "down")
	end

	function volume_widget:mute()
		awesome.emit_signal("dotfiles::volume", "mute")
	end

	return widget
end

return setmetatable(volume_widget, { __call = function(_, ...)
	return create(...)
end })
