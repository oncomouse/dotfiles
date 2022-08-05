local function config_mini()
	-- local augroup = vim.api.nvim_create_augroup("dotfiles-mini.nvim", { clear = true })
	local spec_pair = require("mini.ai").gen_spec.pair
	require("mini.ai").setup({
		custom_textobjects = {
			e = function()
				local from = { line = 1, col = 1 }
				local to = {
					line = vim.fn.line("$"),
					col = vim.fn.getline("$"):len(),
				}
				return { from = from, to = to }
			end,
			s = function(type)
				local punc_matcher = "[.?!]"
				local _, lnum, col, _ = unpack(vim.fn.getpos("."))
				vim.cmd([[normal! )]])
				local p_bottom = {
					line = vim.fn.line("."),
					col = vim.fn.col("."),
				}
				vim.cmd([[normal! (]])
				local p_top = {
					line = vim.fn.line("."),
					col = vim.fn.col("."),
				}
				vim.api.nvim_win_set_cursor(0, { lnum, col })
				local p_orig_bottom = {
					line = p_bottom.line,
					col = p_bottom.col,
				}
				local last_line_of_sentence = vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
				while p_bottom.line >= p_top.line do
					if last_line_of_sentence:sub(p_bottom.col, p_bottom.col):match(punc_matcher) then
						break
					else
						if p_bottom.col == 1 then
							p_bottom.line = p_bottom.line - 1
							last_line_of_sentence =
								vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
							if last_line_of_sentence == nil then
								break
							end
							p_bottom.col = #last_line_of_sentence - 1
						else
							p_bottom.col = p_bottom.col - 1
						end
					end
				end
				if p_bottom.line < p_top.line or (p_bottom.line == p_top.line and p_bottom.col < p_top.col)then
					p_bottom = p_orig_bottom
				end
				if type == "i" then
					if p_bottom.col == 1 then
						p_bottom.line = p_bottom.line - 1
						last_line_of_sentence =
							vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
						p_bottom.col = #last_line_of_sentence - 1
					else
						p_bottom.col = p_bottom.col -1
					end
				end
				-- print(vim.inspect(last_line_of_sentence))
				-- if p_bottom.col ~= #last_line_of_sentence then
				-- 	p_bottom.col = p_bottom.col - (type == "i" and 3 or 2)
				-- else
				-- 	local a,b = string.find(last_line_of_sentence, "[.!?]%s*$")
				-- 	p_bottom.col = p_bottom.col - (b - a + (type == "i" and 1 or 0))
				-- end
				return {
					from = p_top,
					to = p_bottom,
				}
				-- local line = vim.api.nvim_get_current_line()
				-- local end_of_sentence = string.find(string.sub(line, col, #line), punc_matcher)
				-- if not end_of_sentence then
				-- 	return nil
				-- end
				-- end_of_sentence = end_of_sentence + col - 1
				-- local _, start_of_sentence = string.find(string.sub(line, 1, col), punc_matcher .. "%s+[A-Z]")
				-- if start_of_sentence == nil and string.match(string.sub(line, 1, 1), "[A-Z]") then
				-- 	start_of_sentence = 1
				-- end
				-- if not start_of_sentence then
				-- 	return nil
				-- end
				-- end_of_sentence = type == "i" and end_of_sentence - 1 or end_of_sentence
				-- return {
				-- 	from = {
				-- 		line = lnum,
				-- 		col = start_of_sentence,
				-- 	},
				-- 	to = {
				-- 		line = lnum,
				-- 		col = end_of_sentence,
				-- 	},
				-- }
			end,
			["*"] = spec_pair("*", "*", { type = "greedy" }),
			["_"] = spec_pair("_", "_", { type = "greedy" }),
		},
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
	})
	-- vim.api.nvim_create_autocmd("FileType", {
	-- 	pattern = "markdown",
	-- 	group = augroup,
	-- 	callback = function()
	-- 		-- local spec_pair = require("mini.ai").gen_spec.pair
	-- 		-- vim.b.miniai_config = {
	-- 		-- 	custom_textobjects = {
	-- 		-- 		["*"] = spec_pair("*", "*", { type = "greedy" }),
	-- 		-- 		["_"] = spec_pair("_", "_", { type = "greedy" }),
	-- 		-- 	},
	-- 		-- }
	-- 	end,
	-- })
	require("mini.comment").setup({})
	require("mini.indentscope").setup({
		options = {
			indent_at_cursor = false,
		},
	})
	vim.g.miniindentscope_disable = true
	require("mini.jump").setup({})
end

return config_mini
