---@class Pair
---@field line integer
---@field col integer

---@alias ai_type "i" | "a"
return {
	{
		"echasnovski/mini.ai",
		dependencies = {
			{ "oncomouse/vim-textobj-sentence", branch = "lua", dev = true }, -- Sentence object
		},
		event = { "BufReadPre", "BufNewFile" },
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

			---@return Pair #The Pair at the current cursor position
			local function make_point()
				local _, l, c, _ = unpack(vim.fn.getpos("."))
				return {
					line = l,
					col = c,
				}
			end
			
			local Sentence = {}
			function Sentence.select(pattern, count)
				vim.fn.search(pattern, "bc")
				local start = vim.fn.getpos(".")
				local times = 1
				vim.fn.search(vim.b.textobj_sentence_re_i, "ceW", 0, 0, function()
					if times == count then
						return 0
					end
					times = times + 1
					return 1
				end)
				local ed = vim.fn.getpos(".")
				return {
					from = {
						line = start[2],
						col = start[3],
					},
					to = {
						line = ed[2],
						col = ed[3],
					},
				}
			end
			function Sentence.select_a(count)
				if not vim.b.textobj_sentence_re_a then
					require("textobj-sentence").setup()
				end
				return Sentence.select(vim.b.textobj_sentence_re_a, count)
			end
			function Sentence.select_i(count)
				if not vim.b.textobj_sentence_re_i then
					require("textobj-sentence").setup()
				end
				return Sentence.select(vim.b.textobj_sentence_re_i, count)
			end

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
					s = function(type, _, opts)
						return (type == "a" and Sentence.select_i or function(count)
							local results = Sentence.select_i(count)
							local start = (results.from.line == results.to.line and results.from.col or 0)
							local line = vim.api
								.nvim_buf_get_lines(0, results.to.line - 1, results.to.line, false)[1]
								:sub(start, results.to.col)
							results.to.col = start + (line:find("['\".?!]+$") or 0) - 2
							return results
						end)(opts.n_times)
					end,
					S = function(type, _, opts)
						return (type == "a" and Sentence.select_a or Sentence.select_i)(opts.n_times)
					end,

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
				group = vim.api.nvim_create_augroup("dotfiles-miniai-custom-textobjects", {}),
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
				basic = true,
			},
			mappings = {
				move_with_alt = true,
				windows = true,
			},
		},
		config = function(_, opts)
			require("mini.basics").setup(opts)
			vim.keymap.del("t", "<C-w>")
			vim.opt.completeopt:append("preview")
			vim.opt.shortmess:append("Wc")
		end,
	},

	{
		"echasnovski/mini.bracketed",
		event = "VeryLazy",
		config = function(_, opts)
			require("mini.bracketed").setup(opts)
		end,
	},

	-- :Bd[!] for layout-safe bufdelete
	{
		"echasnovski/mini.bufremove",
		event = "VeryLazy",
		config = function(opts)
			require("mini.bufremove").setup(opts)
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
		config = function()
			require("mini.comment").setup({
				hooks = {
					pre = function()
						require("ts_context_commentstring.internal").update_commentstring()
					end,
				},
			})
		end,
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
			require("mini.misc").setup()
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
		config = function(_, opts)
			require("mini.move").setup(opts)
		end
	},

	-- Replace vim-surround:
	{
		"echasnovski/mini.surround",
		keys = function(_, keys)
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
			-- Populate the keys based on the user's options
			local plugin = require("lazy.core.config").spec.plugins["mini.surround"]
			local opts = require("lazy.core.plugin").values(plugin, "opts", false)
			local mappings = {
				{ opts.mappings.add, desc = "Add surrounding", mode = { "n", "v" } },
				{ opts.mappings.delete, desc = "Delete surrounding" },
				{ opts.mappings.find, desc = "Find right surrounding" },
				{ opts.mappings.find_left, desc = "Find left surrounding" },
				{ opts.mappings.highlight, desc = "Highlight surrounding" },
				{ opts.mappings.replace, desc = "Replace surrounding" },
				{ opts.mappings.update_n_lines, desc = "Update `MiniSurround.config.n_lines`" },
			}
			mappings = vim.tbl_filter(function(m)
				return m[1] and #m[1] > 0
			end, mappings)
			return vim.list_extend(mappings, keys)
		end,
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
				highlight = "sh",
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
		end,
	},
}
