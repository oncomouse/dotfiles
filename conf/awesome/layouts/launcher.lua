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
local layout_names = table.concat(
	gears.table.map(function(x)
		return x.name
	end, layouts),
	"\n"
)
local rofi_call = "rofi -location 1 -theme-str 'window {width: 100%;} listview {columns: 3;}' -match fuzzy -auto-select -p Layouts -font '"
	.. beautiful.font
	.. "'"
return function()
	local rofi_cmd = "printf '" .. layout_names .. "' | " .. rofi_call .. " -dmenu"
	awful.spawn.easy_async_with_shell(rofi_cmd, function(stdout)
		stdout = stdout:gsub("^%s*(.-)%s*$", "%1")
		for layout in gears.table.iterate(layouts, function(l)
			return l.name == stdout
		end) do
			awful.layout.set(layout)
		end
	end)
end
