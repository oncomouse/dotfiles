local function config_mini()
	-- vim-textobj-sentence configuration
	vim.g["textobj#sentence#doubleDefault"] = "“”"
	vim.g["textobj#sentence#singleDefault"] = "‘’"
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
		local function get_point(result)
			return {
				line = result[2],
				col = result[3]
			}
		end
		return function(type)
			local results = map["select_function_" .. type]()
			return {
				from = get_point(results[2]),
				to = get_point(results[3]),
			}
		end
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
			s = from_textobject_user({
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
