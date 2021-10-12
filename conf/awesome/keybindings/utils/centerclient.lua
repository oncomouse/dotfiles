local function centerclient(c)
	if not c.floating then
		return
	end
	local cg = c:geometry()
	local sg = c.screen.tiling_area
	local cs = c:struts()
	local move = {
		x = 0,
		y = 0,
		w = 0,
		h = 0,
	}
	local center = {
		x = (sg.width - cs.left - cs.right - cg.width) / 2,
		y = (sg.height - cs.top - cs.bottom - cg.height) / 2,
	}
	move.x = center.x - cg.x
	move.y = center.y - cg.y
	c:relative_move(move.x, move.y, move.w, move.h)
end

return centerclient
