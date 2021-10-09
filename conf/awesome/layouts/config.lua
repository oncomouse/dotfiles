local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")

beautiful.layout_centeredmonocle = gears.color.recolor_image(
	gears.filesystem.get_themes_dir() .. "default/layouts/magnifierw.png",
	beautiful.fg_normal
)

-- Table of layouts to cover with awful.layout.inc, order matters.
beautiful.default_layouts = {
	awful.layout.suit.tile.right,
	awful.layout.suit.tile.left,
	require("layouts.centeredmonocle"),
	awful.layout.suit.floating,
}

