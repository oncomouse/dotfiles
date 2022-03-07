local cairo = require("lgi").cairo
local gears = require("gears")
local beautiful = require("beautiful")

local function make_background()
	local tile_color = gears.color(beautiful.background)
	local box_color = gears.color(beautiful.color8)
	local tile_width = beautiful.background_dot_tile_size
	local box_width = beautiful.background_dot_width

	-- Create the image:
	local img = cairo.ImageSurface.create(cairo.Format.ARGB32, tile_width, tile_width)
	local ctx = cairo.Context(img)
	ctx:save()
	ctx:set_source(tile_color)
	ctx:paint()
	ctx:restore()

	-- Draw two circles to make pattern:
	ctx:set_source(box_color)

	ctx:arc(math.floor(tile_width / 4), math.floor(tile_width / 4), math.floor(box_width / 2), 0, 2 * math.pi)
	ctx:fill()

	ctx:arc(math.floor(3 * tile_width / 4), math.floor(3 * tile_width / 4), math.floor(box_width / 2), 0, 2 * math.pi)
	ctx:fill()

	return img
end

return function(s)
	gears.wallpaper.tiled(make_background(), s)
end
