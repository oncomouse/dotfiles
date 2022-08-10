local function config_mini()
	---@class Pair
	---@field line integer
	---@field col integer

	---@return Pair
	local function make_point()
		local _, l, c, _ = unpack(vim.fn.getpos("."))
		return {
			line = l,
			col = c,
		}
	end

	---@param inside boolean
	---@return { from: Pair, to: Pair }
	local function extract_sentence(type)
		local inside = type == "i"
		-- Get the end of the sentence:
		vim.cmd([[normal! )]])
		local to = make_point()

		-- Get the beginning of the sentence:
		vim.cmd([[normal! (]])
		local from = make_point()

		local punc_matcher = "[.?!]"
		-- Try to find any punctuation at the end of our sentence:
		local target_line = to.line
		while target_line >= from.line and target_line > 0 do
			-- From where should we start searching?
			local start_pos = target_line == from.line and from.col or 0
			-- Get a match, if one exists:
			local match_start, match_stop = vim.regex(punc_matcher):match_line(0, target_line - 1, start_pos)
			if match_start ~= nil then
				to.col = start_pos + (inside and match_start or match_stop)
				to.line = target_line
				break
			end
			target_line = target_line - 1
		end

		return {
			from = from,
			to = to
		}
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
			s = extract_sentence, -- Sentences (using sentence-wise move commands `(` and `)`)
			[","] = { -- Grammatically correct comma matching
				{
					"[%.?!][ ]*()()[^,%.?!]+(),[ ]*()", -- Start of sentence
					"(),[ ]*()[^,%.?!]+()()[%.?!][ ]*", -- End of sentence
					",[ ]*()[^,%.?!]+(),[ ]*", -- Dependent clause
					"^()[A-Z][^,%.?!]+(),[ ]*", -- Start of line
				},
			},
		},
		mappings = {
			around_last = "aN",
			inside_last = "iN",
		},
		search_method = "cover", -- Only use next and last mappings to search
	})

	require("mini.comment").setup({})

	-- We just use this for the indent textobjects:
	require("mini.indentscope").setup({})
	vim.g.miniindentscope_disable = true

	-- Fancy f/F/t/T:
	require("mini.jump").setup({})

	-- Replace vim-surround:
	require("mini.surround").setup({
		custom_surroundings = {
			["("] = { output = { left = "( ", right = " )" } },
			["["] = { output = { left = "[ ", right = " ]" } },
			["{"] = { output = { left = "{ ", right = " }" } },
			["<"] = { output = { left = "< ", right = " >" } },
		},
		mappings = {
			add = "ys",
			delete = "ds",
			find = "",
			find_left = "",
			highlight = "",
			replace = "cs",
			update_n_lines = "",
		},
		search_method = "cover_or_next",
	})
	-- Remap adding surrounding to Visual mode selection
	vim.keymap.del("x", "ys")
	vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
	-- Make special mapping for "add surrounding for line"
	vim.keymap.set("n", "yss", "ys_", { noremap = false })
end

return config_mini
