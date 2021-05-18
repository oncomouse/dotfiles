local cairo = require('lgi').cairo
local gears = require('gears')
local xrdb = require("beautiful").xresources.get_current_theme()

local function make_background()
	local tile_color = gears.color(xrdb.color0)
	local box_color = gears.color(xrdb.color8)
	local tile_width = 125
	local box_width = 4

	-- Create the image:
	local img = cairo.ImageSurface.create(cairo.Format.ARGB32, tile_width, tile_width)
	local ctx  = cairo.Context(img)
	ctx:save()
	ctx:set_source(tile_color)
	ctx:paint()
	ctx:restore()

	-- Draw two circles to make pattern:
	ctx:set_source(box_color)

	ctx:arc(math.floor(tile_width/4), math.floor(tile_width/4), math.floor(box_width/2), 0, 2*math.pi)
	ctx:fill()

	ctx:arc(math.floor(3*tile_width/4), math.floor(3*tile_width/4), math.floor(box_width/2), 0, 2*math.pi)
	ctx:fill()

	return img
end

return function() gears.wallpaper.tiled(make_background()) end
