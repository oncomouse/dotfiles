-- luacheck: globals vim
local utils = require("nvim-autopairs.utils")
local get_buf_rules = require("nvim-autopairs").get_buf_rules

local function end_regex()
	return "["
		.. vim.fn.join(
			vim.tbl_filter(
				function(rule)
					return rule ~= -1
				end,
				vim.tbl_map(function(rule)
					-- Remove endwise rules, which we filter above:
					if rule.is_endwise == true then
						return -1
					end
					if rule.end_pair == "]" then
						return "%]"
					end
					return rule.end_pair
				end, get_buf_rules())
			),
			""
		)
		.. "]"
end

local function jump_autopairs()
	local row, col = utils.get_cursor()
	local line_after_cursor = string.sub(utils.text_get_current_line(0), col)
	if vim.b.end_regex == nil then
		vim.b.end_regex = end_regex()
	end

	-- Go through the after characters, looking for end_pairs:
	local last_pos = 0
	while true do
		local i = string.find(line_after_cursor, vim.b.end_regex, last_pos + 1)
		if i == nil then
			break
		end
		last_pos = i
	end

	-- If we found an autopair, jump to after it:
	if last_pos > 0 then
		vim.api.nvim_win_set_cursor(0, { row + 1, col + last_pos })
	end
end

return jump_autopairs
