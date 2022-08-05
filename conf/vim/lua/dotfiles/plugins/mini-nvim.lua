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
				local function make_point()
					local _, l, c, _ = unpack(vim.fn.getpos("."))
					return {
						line = l,
						col = c,
					}
				end
				-- Store the cursor position:
				local p_cursor = make_point()

				-- Get the end of the sentence:
				vim.cmd([[normal! )]])
				local p_bottom = make_point()
				local p_orig_bottom = make_point()

				-- Get the beginning of the sentence:
				vim.cmd([[normal! (]])
				local p_top = make_point()

				-- Restore the cursor:
				vim.api.nvim_win_set_cursor(0, { p_cursor.line, p_cursor.col - 1 })

				-- Try to find any punctuation at the end of our sentence:
				local last_line_of_sentence = vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
				while p_bottom.line >= p_top.line do
					if last_line_of_sentence:sub(p_bottom.col, p_bottom.col):match(punc_matcher) then
						break
					else -- Iterate to the previous character:
						-- If we're at the start of the line, move up one line:
						if p_bottom.col == 1 then
							p_bottom.line = p_bottom.line - 1
							last_line_of_sentence =
								vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
							-- Guard against hitting the top of the file:
							if last_line_of_sentence == nil then
								break
							end
							p_bottom.col = #last_line_of_sentence - 1
						else -- Otherwise, go back one character:
							p_bottom.col = p_bottom.col - 1
						end
					end
				end

				-- Just use what Neovim gave us if the above didn't work:
				if p_bottom.line < p_top.line or (p_bottom.line == p_top.line and p_bottom.col < p_top.col) then
					p_bottom = p_orig_bottom
				end

				-- Adjust the target if we are selecting "inside" instead of "around":
				if type == "i" then
					-- But only if there is actually a punctuation mark found:
					if vim.fn.getline(p_bottom.line):sub(p_bottom.col, p_bottom.col):match(punc_matcher) then
						if p_bottom.col == 1 then
							p_bottom.line = p_bottom.line - 1
							last_line_of_sentence =
								vim.api.nvim_buf_get_lines(0, p_bottom.line - 1, p_bottom.line, false)[1]
							p_bottom.col = #last_line_of_sentence - 1
						else
							p_bottom.col = p_bottom.col - 1
						end
					end
				end

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
