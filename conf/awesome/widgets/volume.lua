-- luacheck: globals awesome
local awful = require("awful")
local block = require("utils.block")

local volume_widget = {}

local function create()
	local widget, signal = block([[
	/bin/sh -c '
	function change {
		local vol_change=${2:-5}
		case "$1" in
		  "up")
			if pamixer --get-mute > /dev/null; then
			  pamixer -u
			else
			  pamixer -i "$vol_change"
			fi
			;;
		  "down")
			if pamixer --get-mute > /dev/null; then
			  pamixer -u
			else
			  pamixer -d "$vol_change"
			fi
			;;
		  "mute")
			pamixer -t > /dev/null
			;;
		  *)
			;;
		esac
	}
	case $BUTTON in
		1) change mute ;;
		4) change down ;;
		5) change up ;;
	esac
	icon="墳"
	output="$(pamixer --get-volume-human | sed s/muted/x/g)"
	if [ ${#output} -gt 0 ]; then
		echo "$icon $output"
	fi
	'
	]])

	awesome.connect_signal("dotfiles::volume", function(direction)
		awful.spawn.easy_async_with_shell("~/Projects/dwm-config/scripts/volume.sh ".. direction, function()
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
