local function move_resize(move)
	return function(c)
		if c.floating then
			c:relative_move(move.x or 0, move.y or 0, move.w or 0, move.h or 0)
		end
	end
end

return move_resize
