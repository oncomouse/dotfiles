local beautiful = require("beautiful")
local function moveresizeedge(direction, resize)
	return function(c)
		if not c.floating then
			if beautiful.toggle_floating_on_moveresize then
				require("awful").client.floating.toggle(c)
			else
				return
			end
		end
		local move = {
			x = 0,
			y = 0,
			w = 0,
			h = 0,
		}
		local cg = c:geometry()
		local sg = c.screen.tiling_area
		local cs = c:struts()
		if direction == "Left" then
			move.x = sg.x - cg.x - cs.left
			if resize then
				move.w = -1 * move.x
			end
		elseif direction == "Right" then
			move[resize and "w" or "x"] = sg.x + sg.width - cg.x - cg.width - cs.right - beautiful.border_width * 2
		elseif direction == "Up" then
			move.y = sg.y - cg.y - cs.top
			if resize then
				move.h = -1 * move.y
			end
		elseif direction == "Down" then
			move[resize and "h" or "y"] = sg.y + sg.height - cg.y - cg.height - cs.bottom - beautiful.border_width * 2
		end
		c:relative_move(move.x or 0, move.y or 0, move.w or 0, move.h or 0)
	end
end

return moveresizeedge
