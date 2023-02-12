---@class Pair
---@field line integer
---@field col integer

---@alias ai_type "i" | "a"
return {
	{
		"echasnovski/mini.ai",
		dependencies = {
			{ "preservim/vim-textobj-sentence" }, -- Sentence object
		},
		event = "VeryLazy",
		opts = function()
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

			---@param map { [string]: fun() } A list of textobject functions
			---@return fun(type:ai_type):Pair
			local function from_textobject_user(map)
				local function textobject_result_to_point(result)
					return {
						line = result[2],
						col = result[3],
					}
				end

				---@param type ai_type
				---@return Pair
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

			---@return Pair #The Pair at the current cursor position
			local function make_point()
				local _, l, c, _ = unpack(vim.fn.getpos("."))
				return {
					line = l,
					col = c,
				}
			end

			-- local function get_indentation(line)
			-- 	return vim.api.nvim_buf_get_lines(0, line - 1, line, false)[1]:match("^%s+")
			-- end
			--
			-- local function check_indentation(line, target)
			-- 	if target == nil then
			-- 		return true
			-- 	end
			-- 	local l = vim.api.nvim_buf_get_lines(0, line - 1, line, false)[1]
			-- 	if #l == 0 then
			-- 		return true
			-- 	end
			-- 	return l:sub(1, #target) == target
			-- end
			--
			return {
				custom_textobjects = {

					e = function() -- Whole buffer
						local from = { line = 1, col = 1 }
						local last_line_length = #vim.fn.getline("$")
						local to = {
							line = vim.fn.line("$"),
							col = last_line_length == 0 and 1 or last_line_length,
						}
						return {
							from = from,
							to = to,
						}
					end,

					-- i = function(type) -- Indentation
					-- 	local _, line, _, _ = unpack(vim.fn.getpos("."))
					-- 	local target_indentation = get_indentation(line)
					-- 	local start = line - 1
					-- 	local stop = line + 1
					-- 	local _, end_line, _, _ = unpack(vim.fn.getpos("$"))
					-- 	while true do
					-- 		if start == 0 then
					-- 			start = 1
					-- 			break
					-- 		end
					-- 		if not check_indentation(start, target_indentation) then
					-- 			start = start + 1
					-- 			break
					-- 		end
					-- 		start = start - 1
					-- 	end
					-- 	while true do
					-- 		if stop == end_line + 1 then
					-- 			stop = end_line
					-- 			break
					-- 		end
					-- 		if not check_indentation(stop, target_indentation) then
					-- 			stop = stop - 1
					-- 			break
					-- 		end
					-- 		stop = stop + 1
					-- 	end
					-- 	if type == "a" then
					-- 		start = start == 1 and 1 or start - 1
					-- 		stop = stop == end_line and end_line or stop + 1
					-- 	end
					-- 	return {
					-- 		to = {
					-- 			line = start,
					-- 			col = 1,
					-- 		},
					-- 		from = {
					-- 			line = stop,
					-- 			col = #vim.api.nvim_buf_get_lines(0, stop - 1, stop, false)[1] + 1,
					-- 		},
					-- 	}
					-- end,

					z = function(type) -- Folds
						vim.api.nvim_feedkeys("[z" .. (type == "i" and "j0" or ""), "x", true)
						local from = make_point()
						vim.api.nvim_feedkeys("]z" .. (type == "i" and "k$" or "$"), "x", true)
						local to = make_point()

						return {
							from = from,
							to = to,
						}
					end,

					-- Sentence objects, based on vim-textobj-sentence and how vim-textobj-function works:
					--   `is` grabs the words of the sentence (without trailing punctuation and quotes)
					--   `as` grabs ending punctuation but not white space
					--   `iS` works like `as`
					--   `aS` grabs ending punctuation and white space
					s = from_textobject_user({
						select_function_i = function() -- Rewrite selection results to remove trailing punctuation and quotes
							local results = vim.fn["textobj#sentence#select_i"]()
							local start = (results[2][2] == results[3][2] and results[2][3] or 0)
							local line = vim.api
								.nvim_buf_get_lines(0, results[3][2] - 1, results[3][2], false)[1]
								:sub(start, results[3][3])
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
				},
				mappings = {
					around_last = "aN",
					inside_last = "iN",
				},
				n_lines = 50,
				search_method = "cover", -- Only use next and last mappings to search
			}
		end,
		config = function(_, opts)
			require("mini.ai").setup(opts)
			-- Per-file textobjects:
			local spec_pair = require("mini.ai").gen_spec.pair
			local custom_textobjects = {
				lua = {
					["s"] = spec_pair("[[", "]]"),
				},
				markdown = {
					["*"] = spec_pair("*", "*", { type = "greedy" }), -- Grab all asterisks when selecting
					["_"] = spec_pair("_", "_", { type = "greedy" }), -- Grab all underscores when selecting
					["l"] = { "%b[]%b()", "^%[().-()%]%([^)]+%)$" }, -- Link targeting name
					["L"] = { "%b[]%b()", "^%[.-%]%(()[^)]+()%)$" }, -- Link targeting href
				},
			}
			vim.api.nvim_create_autocmd("FileType", {
				group = "dotfiles-settings",
				pattern = vim.fn.join(vim.tbl_keys(custom_textobjects), ","),
				callback = function()
					local ft = vim.opt.filetype:get()
					vim.b.miniai_config = {
						custom_textobjects = custom_textobjects[ft],
					}
				end,
			})
		end,
	},

	-- ga,gA for alignment:
	{
		"echasnovski/mini.align",
		keys = {
			{ "ga", mode = { "n", "x" } },
			{ "gA", mode = { "n", "x" } },
		},
	},

	{
		"echasnovski/mini.basics",
		lazy = false,
		opts = {
			options = {
				basic = false,
			},
			mappings = {
				move_with_alt = true,
				windows = true,
			},
		},
		config = function(_, opts)
			require("mini.basics").setup(opts)
			vim.keymap.del("t", "<C-w>")
		end,
	},

	-- :Bd[!] for layout-safe bufdelete
	{
		"echasnovski/mini.bufremove",
		event = "VeryLazy",
		config = function()
			vim.api.nvim_create_user_command("Bd", function(args)
				require("mini.bufremove").delete(0, not args.bang)
			end, {
				bang = true,
			})
		end,
	},

	-- gc for commenting/uncommenting:
	{
		"echasnovski/mini.comment",
		keys = {
			{ "gc", mode = { "o", "n", "x" } },
			{ "gcc", mode = "n" },
		},
	},

	{
		"echasnovski/mini.indentscope",
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			-- symbol = "▏",
			symbol = "│",
			options = { try_as_border = true },
		},
		config = function(_, opts)
			vim.api.nvim_create_autocmd("FileType", {
				pattern = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" },
				callback = function()
					vim.b.miniindentscope_disable = true
				end,
			})
			opts.draw = {
				animation = require("mini.indentscope").gen_animation.none(),
			}
			require("mini.indentscope").setup(opts)
		end,
	},

	{
		"echasnovski/mini.misc",
		event = { "BufReadPre", "BufNewFile" },
		config = function()
			require('mini.misc').setup()
			require("mini.misc").setup_auto_root({
				".git",
				"Gemfile",
				"Makefile",
				"Rakefile",
				"package.json",
				"pyproject.toml",
				"setup.py",
				".project-root",
			})
			require("mini.misc").setup_restore_cursor()
		end,
	},

	{
		"echasnovski/mini.move",
		keys = {
			{ "<M-h>", mode = { "n", "v" } },
			{ "<M-j>", mode = { "n", "v" } },
			{ "<M-k>", mode = { "n", "v" } },
			{ "<M-l>", mode = { "n", "v" } },
		},
	},

	-- Replace vim-surround:
	{
		"echasnovski/mini.surround",
		lazy = false,
		opts = {
			custom_surroundings = {
				["q"] = {
					input = { "“().-()”" },
					output = { left = "“", right = "”" },
				},
			},
			mappings = {
				add = "ys",
				delete = "ds",
				find = "sf",
				find_left = "sF",
				highlight = "hs",
				replace = "cs",
				update_n_lines = "",
				suffix_last = "N",
				suffix_next = "n",
			},
			n_lines = 50,
			search_method = "cover_or_next",
		},
		config = function(_, opts)
			require("mini.surround").setup(opts)
			-- Remap adding surrounding to Visual mode selection
			vim.keymap.del("x", "ys")
			vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
			-- Make special mapping for "add surrounding for line"
			vim.keymap.set("n", "yss", "ys_", { noremap = false })

			--Per-file surroundings:
			local custom_surroundings = {
				lua = {
					s = {
						input = { "%[%[().-()%]%]" },
						output = { left = "[[", right = "]]" },
					},
				},
				markdown = {
					["B"] = { -- Surround for bold
						input = { "%*%*().-()%*%*" },
						output = { left = "**", right = "**" },
					},
					["I"] = { -- Surround for italics
						input = { "%*().-()%*" },
						output = { left = "*", right = "*" },
					},
					["L"] = {
						input = { "%[().-()%]%([^)]+%)" },
						output = function()
							local href = require("mini.surround").user_input("Href")
							return {
								left = "[",
								right = "](" .. href .. ")",
							}
						end,
					},
				},
			}
			vim.api.nvim_create_autocmd("FileType", {
				group = "dotfiles-settings",
				pattern = vim.fn.join(vim.tbl_keys(custom_surroundings), ","),
				callback = function()
					local ft = vim.opt.filetype:get()
					vim.b.minisurround_config = {
						custom_surroundings = custom_surroundings[ft],
					}
				end,
			})
		end,
	},
}
