local function config_mini()
	local queries = require("nvim-treesitter.query")

	local miniAiTreesitter = function(ai_type, _, _, query_list)
		ai_type = ai_type == "a" and ".outer" or ".inner"
		query_list = vim.tbl_map(function(query)
			return query .. ai_type
		end, query_list)

		local matches = {}
		for _, query in pairs(query_list) do
			vim.list_extend(matches, queries.get_capture_matches_recursively(0, query, "textobjects"))
		end

		matches = vim.tbl_map(function(match)
			local from_line, from_col, to_line, to_col = match.node:range()
			return {
				from = { line = from_line + 1, col = from_col + 1 },
				to = { line = to_line + 1, col = to_col + 1 },
			}
		end, matches)

		return matches
	end

	local miniAiTreeWrapper = function(query_list)
		if type(query_list) ~= "table" then
			query_list = { query_list }
		end
		return function(ai_type, _, opts)
			return miniAiTreesitter(ai_type, _, opts, query_list)
		end
	end

	-- vim-textobj-sentence configuration
	-- vim.g["textobj#sentence#doubleDefault"] = "“”"
	-- vim.g["textobj#sentence#singleDefault"] = "‘’"
	vim.g["textobj#sentence#doubleDefault"] = [[""]]
	vim.g["textobj#sentence#singleDefault"] = [['']]
	vim.g["textobj#sentence#abbreviations"] = {
		"[ABCDIMPSUabcdegimpsv]",
		"l[ab]",
		"[eRr]d",
		"Ph",
		"[Ccp]l",
		"[Lli]n",
		"[cn]o",
		"[Oe]p",
		"[DJMSh]r",
		"[MVv]s",
		"[CFMPScfpw]t",
		"alt",
		"[Ee]tc",
		"div",
		"es[pt]",
		"[Ll]td",
		"min",
		"[MD]rs",
		"[Aa]pt",
		"[Aa]ve?",
		"[Ss]tr?",
		"[Aa]ssn",
		"[Bb]lvd",
		"[Dd]ept",
		"incl",
		"Inst",
		"Prof",
		"Univ",
		"Messrs",
	}

	local function from_textobject_user(map)
		local function textobject_result_to_point(result)
			return {
				line = result[2],
				col = result[3],
			}
		end

		return function(type)
			local results = map["select_function_" .. type]()
			if results == 0 then
				return nil
			end
			return {
				from = textobject_result_to_point(results[2]),
				to = textobject_result_to_point(results[3]),
			}
		end
	end

	local function make_point()
		local _, l, c, _ = unpack(vim.fn.getpos("."))
		return {
			line = l,
			col = c,
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

			z = function(type) -- Folds
				vim.cmd("normal [z" .. (type == "i" and "j0" or ""))
				local from = make_point()
				vim.cmd("normal ]z" .. (type == "i" and "k$" or "$"))
				local to = make_point()

				return {
					from = from,
					to = to,
				}
			end,

			-- These sentence objects are based on textobject-function:
			--   `is` grabs the words of the sentence (without trailing punctuation and quotes)
			--   `as` grabs ending punctuation but not white space
			--   `iS` works like `as`
			--   `aS` grabs ending punctuation and white space
			s = from_textobject_user({
				select_function_i = function() -- Rewrite selection results to remove trailing punctuation and quotes
					local results = vim.fn["textobj#sentence#select_i"]()

					local start = (results[2][2] == results[3][2] and results[2][3] or 0)
					local line = vim.api.nvim_buf_get_lines(0, results[3][2] - 1, results[3][2], false)[1]:sub(
						start,
						results[3][3]
					)
					results[3][3] = start + line:find("['\".?!]+$") - 2
					return results
				end,
				select_function_a = vim.fn["textobj#sentence#select_i"],
			}),
			S = from_textobject_user({
				select_function_a = vim.fn["textobj#sentence#select_a"],
				select_function_i = vim.fn["textobj#sentence#select_i"],
			}),

			[","] = { -- Grammatically correct comma matching
				{
					"[%.?!][ ]*()()[^,%.?!]+(),[ ]*()", -- Start of sentence
					"(),[ ]*()[^,%.?!]+()()[%.?!][ ]*", -- End of sentence
					",[ ]*()[^,%.?!]+(),[ ]*", -- Dependent clause
					"^()[A-Z][^,%.?!]+(),[ ]*", -- Start of line
				},
			},

			o = miniAiTreeWrapper({ "@block", "@conditional", "@loop" }),
			f = miniAiTreeWrapper({ "@function", "@class" }),
			c = miniAiTreeWrapper("@comment"),
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
	vim.keymap.set("n", "<esc>", function() -- Use <esc> to stop jumping, if we enter the wrong target
		if require("mini.jump").state.jumping then
			require("mini.jump").stop_jumping()
		end
		return "<esc>"
	end, { expr = true, noremap = false })

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
	-- Custom mappings:
	--  ~/dotfiles/conf/vim/after/ftplugin/lua.lua
	--  ~/dotfiles/conf/vim/after/ftplugin/markdown.lua
end

return config_mini
