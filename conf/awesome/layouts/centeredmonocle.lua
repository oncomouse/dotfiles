---------------------------------------------------------------------------
--- Maximized and fullscreen layouts module for awful
--
-- @author Julien Danjou &lt;julien@danjou.info&gt;
-- @copyright 2008 Julien Danjou
-- @module awful.layout
---------------------------------------------------------------------------
local beautiful = require("beautiful")

-- Grab environment we need
local pairs = pairs

local cm = {}

--- The max layout layoutbox icon.
-- @beautiful beautiful.layout_max
-- @param surface
-- @see gears.surface

--- The fullscreen layout layoutbox icon.
-- @beautiful beautiful.layout_fullscreen
-- @param surface
-- @see gears.surface

local function fmax(p, fs)
	-- Fullscreen?
	local area
	local scale = beautiful.cm_scale or 0.70
	if fs then
		area = p.geometry
	else
		area = p.workarea
	end

	for _, c in pairs(p.clients) do
		local g = {
			x = area.x + area.width * ((1 - scale) / 2),
			y = area.y,
			width = area.width * scale,
			height = area.height,
		}
		p.geometries[c] = g
		c.border_width = 0
	end
end

--- Maximized layout.
-- @clientlayout centeredmonocle
cm.name = "centeredmonocle"
function cm.arrange(p)
	return fmax(p, false)
end
-- selene: allow(unused_variable)
function cm.skip_gap(nclients, t)
	return true
end

return cm
