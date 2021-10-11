local function moveresize(move)
	return function(c)
		if not c.floating then
			if require("beautiful").toggle_floating_on_moveresize then
				require("awful").client.floating.toggle(c)
			else
				return
			end
		end
		c:relative_move(move.x or 0, move.y or 0, move.w or 0, move.h or 0)
	end
end

return moveresize
