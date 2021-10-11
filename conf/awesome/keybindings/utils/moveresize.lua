local function moveresize(move)
	return function(c)
		if not c.floating then
			return
			-- require("awful").client.floating.toggle(c)
		end
		c:relative_move(move.x or 0, move.y or 0, move.w or 0, move.h or 0)
	end
end

return moveresize
