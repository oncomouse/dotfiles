local house_pos
return {
	s(
		{
			trig = "house",
		},
		fmt(
			[[
	{}{{
		house: '{}',
		clue: '{}',
	}},]],
			{
				f(function(_, _, _)
					if house_pos[2] and house_pos[2] > 0 then
						local line = vim.api.nvim_buf_get_lines(0, house_pos[1], house_pos[1] + 1, false)
						if line[1]:sub(house_pos[2], house_pos[2]) == "," then
							return { "", "" }
						end
					end
					return ""
				end),
				i(1),
				i(2),
			}
		),
		{
			callbacks = {
				[-1] = {
					[events.pre_expand] = function(_, opts)
						house_pos = opts.expand_pos
						-- local pos = opts.expand_pos
						-- if pos[2] and pos[2] > 0 then
						-- 	local line = vim.api.nvim_buf_get_lines(0, pos[1], pos[1] + 1, false)[1]
						-- 	if line:sub(pos[2], pos[2]) == "," then
						-- 		vim.api.nvim_buf_set_text(0, pos[1], 0, pos[1], #line, { line, "" })
						-- 	end
						-- end
					end,
				},
			},
		}
	),
}
