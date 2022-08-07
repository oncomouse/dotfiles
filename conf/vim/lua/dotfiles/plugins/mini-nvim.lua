---@class Pair
---@field line integer
---@field col integer

local function config_mini()

	---@return Pair
	local function make_point()
		local _, l, c, _ = unpack(vim.fn.getpos("."))
		return {
			line = l,
			col = c,
		}
	end

	---@param inside boolean
	---@return Pair top, Pair bottom
	local function extract_sentence(inside)
		-- Get the end of the sentence:
		vim.cmd([[normal! )]])
		local p_bottom = make_point()

		-- Get the beginning of the sentence:
		vim.cmd([[normal! (]])
		local p_top = make_point()

		local punc_matcher = "[.?!]"
		-- Try to find any punctuation at the end of our sentence:
		local target_line = p_bottom.line
		while target_line >= p_top.line and target_line > 0 do
			-- From where should we start searching?
			local start_pos = target_line == p_top.line and p_top.col or 0
			-- Get a match, if one exists:
			local match_start, match_stop = vim.regex(punc_matcher):match_line(0, target_line - 1, start_pos)
			if match_start ~= nil then
				p_bottom.col = start_pos + (inside and match_start or match_stop)
				p_bottom.line = target_line
				break
			end
			target_line = target_line - 1
		end

		return p_top, p_bottom
	end

	require("mini.ai").setup({
		custom_textobjects = {
			e = function() -- Whole buffer
				local from = { line = 1, col = 1 }
				local to = {
					line = vim.fn.line("$"),
					col = vim.fn.getline("$"):len(),
				}
				return {
					from = from,
					to = to,
				}
			end,
			s = function(type) -- Sentences (using sentence-wise move commands `(` and `)`)
				local from, to = extract_sentence(type == "i")

				return {
					from = from,
					to = to,
				}
			end,
			[","] = { -- Grammatically correct comma matching
				{
					"[%.?!][ ]*()()[^,%.?!]+(),[ ]*()", -- Start of sentence
					"(),[ ]*()[^,%.?!]+()()[%.?!][ ]*", -- End of sentence
					",[ ]*()[^,%.?!]+(),[ ]*", -- Dependent clause
					"^()[A-Z][^,%.?!]+(),[ ]*", -- Start of line
				}
			},
		},
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
		search_method = "cover", -- Only use next and last mappings to search
	})
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
