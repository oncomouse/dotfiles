local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")

local layouts = {
	awful.layout.suit.corner.nw,
	awful.layout.suit.corner.ne,
	awful.layout.suit.corner.sw,
	awful.layout.suit.corner.se,
	awful.layout.suit.fair,
	awful.layout.suit.fair.horizontal,
	awful.layout.suit.floating,
	require("layouts.centeredmonocle"),
	awful.layout.suit.magnifier,
	awful.layout.suit.max,
	awful.layout.suit.max.fullscreen,
	awful.layout.suit.spiral,
	awful.layout.suit.spiral.dwindle,
	awful.layout.suit.tile.right,
	awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	awful.layout.suit.tile.top,
}

local function layout_menu()
	awful.menu(gears.table.map(function(layout)
		return {
			layout.name,
			function()
				awful.layout.set(layout)
			end,
			beautiful["layout_" .. layout.name],
		}
	end, layouts)):show()
end

return layout_menu
