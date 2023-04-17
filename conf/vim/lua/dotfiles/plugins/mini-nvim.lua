---@class Pair,
---@field line integer
---@field col integer

---@alias ai_type "i" | "a"
return {
	{
		"echasnovski/mini.basics",
		lazy = false,
		opts = {
			mappings = {
				move_with_alt = true,
				windows = true, -- Move <C-hjkl>; Resize <C-movearrows>
			},
		},
		config = function(_, opts)
			require("mini.basics").setup(opts)
			vim.keymap.del("t", "<C-w>")
			vim.opt.completeopt:append("preview")
			vim.opt.shortmess:append("Wc")
			vim.opt.undofile = false
		end,
	},
	{
		"echasnovski/mini.ai",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			{ "oncomouse/vim-textobj-sentence", branch = "lua", dev = false }, -- Sentence object
		},
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

					-- e = function() -- Whole buffer
					-- 	local from = { line = 1, col = 1 }
					-- 	local last_line_length = #vim.fn.getline("$")
					-- 	local to = {
					-- 		line = vim.fn.line("$"),
					-- 		col = last_line_length == 0 and 1 or last_line_length,
					-- 	}
					-- 	return {
					-- 		from = from,
					-- 		to = to,
					-- 	}
					-- end,

					q = {
						"“().-()”",
						"‘().-()’",
					},

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
			-- return
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
				group = vim.api.nvim_create_augroup("dotfiles-mini_ai", {}),
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
			{ "ga", mode = { "x", "n" }, desc = "Align" },
			{ "gA", mode = { "x", "n" }, desc = "Align with preview" },
		},
		config = function(_, opts)
			require("mini.align").setup(opts)
		end,
	},
	-- Paired commands such as [q/]q
	{
		"echasnovski/mini.bracketed",
		keys = function(_, keys)
			local plugin = require("lazy.core.config").spec.plugins["mini.bracketed"]
			local opts = require("lazy.core.plugin").values(plugin, "opts", false)
			local default_mappings = {
				buffer = "b",
				comment = "c",
				conflict = "x",
				diagnostic = "d",
				file = "f",
				indent = "i",
				jump = "j",
				location = "l",
				oldfile = "o",
				quickfix = "q",
				treesitter = "t",
				undo = "u",
				window = "w",
				yank = "y",
			}
			local mappings = {}
			for type, default in pairs(default_mappings) do
				local map = opts[type] or default
				for _, direction in ipairs({ "[", "]" }) do
					table.insert(mappings, { string.format("%s%s", direction, map) })
					table.insert(mappings, { string.format("%s%s", direction, map:upper()) })
				end
			end

			return vim.list_extend(mappings, keys)
		end,
		config = function(_, opts)
			require("mini.bracketed").setup(opts)
			vim.keymap.set("n", "[t", "<cmd>tabprevious<cr>", {})
			vim.keymap.set("n", "]t", "<cmd>tabnext<cr>", {})
			vim.keymap.set("n", "[T", "<cmd>tabfirst<cr>", {})
			vim.keymap.set("n", "]T", "<cmd>tablast<cr>", {})
		end,
	},
	-- :Bd[!] for layout-safe bufdelete
	{
		"echasnovski/mini.bufremove",
		command = "Bd",
		init = function()
			vim.api.nvim_create_user_command("Bd", function(args)
				require("mini.bufremove").delete(0, not args.bang)
			end, {
				bang = true,
			})
		end,
		config = function(_, opts)
			require("mini.bufremove").setup(opts)
		end,
	},
	-- gc for Comments
	{
		"echasnovski/mini.comment",
		keys = {
			{ "gc", mode = { "o" }, desc = "Comment textobject" },
			{ "gc", mode = { "x" }, desc = "Comment selection" },
			{ "gc", mode = { "n" }, desc = "Comment" },
			{ "gcc", desc = "Comment line" },
		},
		opts = {
			hooks = {
				pre = function()
					require("ts_context_commentstring.internal").update_commentstring({})
				end,
			},
		},
		config = function(_, opts)
			require("mini.comment").setup(opts)
		end,
	},
	-- Show current indentation context:
	{
		"echasnovski/mini.indentscope",
		event = "VeryLazy",
		opts = function()
			return {
				symbol = "│",
				options = {
					indent_at_cursor = false,
					try_as_border = true,
				},
				draw = {
					animation = require("mini.indentscope").gen_animation.none(),
				},
				mappings = {
					object_scope = "ii",
					object_scope_with_border = "ai",
				},
			}
		end,
		init = function()
			vim.api.nvim_create_autocmd("FileType", {
				group = vim.api.nvim_create_augroup("dotfiles-mini_indentscope", {}),
				pattern = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" },
				callback = function()
					vim.b.miniindentscope_disable = true
				end,
			})
		end,
		config = function(_, opts)
			require("mini.indentscope").setup(opts)
		end,
	},
	-- Move lines with alt + hjkl:
	{
		"echasnovski/mini.move",
		keys = function(_, keys)
			local plugin = require("lazy.core.config").spec.plugins["mini.move"]
			local opts = require("lazy.core.plugin").values(plugin, "opts", false)
			local default_mappings = {
				-- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
				left = "<M-h>",
				right = "<M-l>",
				down = "<M-j>",
				up = "<M-k>",

				-- Move current line in Normal mode
				line_left = "<M-h>",
				line_right = "<M-l>",
				line_down = "<M-j>",
				line_up = "<M-k>",
			}
			local mappings = {}
			for name, map in pairs(default_mappings) do
				local m = (opts.mappings and opts.mappings[name]) or map
				local mode = string.find("^line_", name) and "x" or "n"
				table.insert(mappings, { m, mode = { mode } })
			end

			return vim.list_extend(mappings, keys)
		end,
		config = function(_, opts)
			require("mini.move").setup(opts)
		end,
	},

	-- use gS to split and join items in a list:
	{
		"echasnovski/mini.splitjoin",
		keys = {
			{ "gS", mode = { "o", "x", "n" } },
		},
		config = function(_, opts)
			require("mini.splitjoin").setup(opts)
		end,
	},
	{
		"echasnovski/mini.misc",
		lazy = false,
		config = function(_, opts)
			-- Miscellaneous operations:
			require("mini.misc").setup(opts)
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
		"echasnovski/mini.statusline",
		dependencies = { "oncomouse/czs.nvim" },
		lazy = false,
		opts = function()
			local DotfilesStatusline = require("dotfiles.statusline")
			return {
				content = {
					active = function()
						local _, mode_hl = require("mini.statusline").section_mode({ trunc_width = 120 })
						local icon = DotfilesStatusline.section_icon({ trunc_width = 75 })
						local filename = DotfilesStatusline.section_filename({ trunc_width = 140 })
						local fileinfo = DotfilesStatusline.section_fileinfo({ trunc_width = 120 })
						local location = DotfilesStatusline.section_location({ trunc_width = 75 })
						local diagnostics = DotfilesStatusline.section_diagnostics({ trunc_width = 75 })
						local luasnip = DotfilesStatusline.section_luasnip({ trunc_width = 75 })
						local macro = DotfilesStatusline.section_macro()
						local showcmd = DotfilesStatusline.section_showcmd()
						local wordcount = DotfilesStatusline.section_wordcount()
						local search = DotfilesStatusline.section_search({ trunc_width = 75 })

						return require("mini.statusline").combine_groups({
							"%<", -- Mark general truncate point
							{ hl = mode_hl, strings = { icon } },
							{ hl = mode_hl, strings = { " ", filename, " " } },
							{ hl = "MiniStatuslineLuaSnip", strings = { luasnip } },
							{ hl = "MiniStatuslineMacro", strings = { macro } },
							{ hl = "Statusline", strings = { "%=" } }, -- End left alignment
							{ hl = "MiniStatuslineShowcmd", strings = { showcmd } },
							{ hl = "MiniStatuslineWordcount", strings = { wordcount } },
							{ hl = "MiniStatuslineSearch", strings = { search } },
							location,
							{ hl = "MiniStatuslineFileinfo", strings = { " ", fileinfo, " " } },
							diagnostics,
						})
					end,
					inactive = function()
						local filename = DotfilesStatusline.section_filename({ trunc_width = 140, inactive = true })
						return require("mini.statusline").combine_groups({
							{ hl = "StatuslineNC", strings = { filename } },
						})
					end,
				},
			}
		end,
		config = function(_, opts)
			-- Override function used to make statsuline:
			require("mini.statusline").combine_groups = function(groups)
				local parts = vim.tbl_map(function(s)
					if type(s) == "string" then
						return s
					end
					if type(s) ~= "table" then
						return ""
					end

					local string_arr = vim.tbl_filter(function(x)
						return type(x) == "string" and x ~= ""
					end, s.strings or {})
					local str = table.concat(string_arr, "")

					-- Use previous highlight group
					if s.hl == nil then
						return ("%s"):format(str)
					end

					-- Allow using this highlight group later
					if str:len() == 0 then
						return string.format("%%#%s#", s.hl)
					end

					return string.format("%%#%s#%s", s.hl, str)
				end, groups)

				return table.concat(parts, "")
			end
			require("mini.statusline").setup(opts)
		end,
	},
	-- Use cs/ys/ds to manipulate surrounding delimiters:
	{
		"echasnovski/mini.surround",
		opts = {
			custom_surroundings = {
				["q"] = {
					input = {
						{ "“().-()”", "‘().-()’" },
						{ "“().-()”", "‘().-()’" },
					},
					output = { left = "“", right = "”" },
				},

				["Q"] = {
					input = { "‘().-()’" },
					output = { left = "‘", right = "’" },
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
		keys = function(_, keys)
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
		init = function()
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
				group = vim.api.nvim_create_augroup("dotfiles-mini_surround", {}),
				pattern = vim.fn.join(vim.tbl_keys(custom_surroundings), ","),
				callback = function()
					local ft = vim.opt.filetype:get()
					vim.b.minisurround_config = {
						custom_surroundings = custom_surroundings[ft],
					}
				end,
			})
		end,
		config = function(_, opts)
			require("mini.surround").setup(opts)
			-- Remap adding surrounding to Visual mode selection
			vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
			-- Make special mapping for "add surrounding for line"
			vim.keymap.set("n", "yss", "ys_", { noremap = false })
		end,
	},
	{
		"echasnovski/mini.tabline",
		lazy = false,
		opts = {
			set_vim_settings = false,
			tabpage_section = 'none',
		},
		config = function(_, opts)
			require("mini.tabline").setup(opts)
		end,
	},
}
