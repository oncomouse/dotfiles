---@class Pair,
---@field line integer
---@field col integer

---@alias ai_type "i" | "a"

return {
	"echasnovski/mini.nvim",
	config = function()
		-- mini.basics
		require("mini.basics").setup({
			mappings = {
				move_with_alt = true,
				windows = true, -- Move <C-hjkl>; Resize <C-movearrows>
			},
		})
		vim.opt.completeopt:append("preview")
		vim.opt.shortmess:append("Wc")
		vim.opt.undofile = false

		-- mini.ai
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
				vim.fn["textobj#sentence#init"]()
			end
			return Sentence.select(vim.b.textobj_sentence_re_a, count)
		end

		function Sentence.select_i(count)
			if not vim.b.textobj_sentence_re_i then
				vim.fn["textobj#sentence#init"]()
			end
			return Sentence.select(vim.b.textobj_sentence_re_i, count)
		end

		require("mini.ai").setup({
			custom_textobjects = {

				e = function() -- Whole buffer
					local from = { line = 1, col = 1 }
					local last_line_length = #vim.fn.getline("$")
					local to = {
						line = vim.fn.line("$"),
						col = last_line_length == 0 and 1 or last_line_length,
					}
					return { from = from, to = to, vis_mode = "V" }
				end,

				-- Treesitter objects from LazyVim:
				o = require("mini.ai").gen_spec.treesitter({
					a = { "@block.outer", "@conditional.outer", "@loop.outer" },
					i = { "@block.inner", "@conditional.inner", "@loop.inner" },
				}, {}),
				f = require("mini.ai").gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }, {}),
				c = require("mini.ai").gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }, {}),

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
		})

		-- Document in which-key.nvim
		local i = {
			[" "] = "Whitespace",
			[","] = "Comma",
			['"'] = 'Balanced "',
			["'"] = "Balanced '",
			["`"] = "Balanced `",
			["("] = "Balanced (",
			[")"] = "Balanced ) including white-space",
			[">"] = "Balanced > including white-space",
			["<lt>"] = "Balanced <",
			["]"] = "Balanced ] including white-space",
			["["] = "Balanced [",
			["}"] = "Balanced } including white-space",
			["{"] = "Balanced {",
			["?"] = "User Prompt",
			_ = "Underscore",
			a = "Argument",
			b = "Balanced ), ], }",
			c = "Class",
			f = "Function",
			o = "Block, conditional, loop",
			q = "Smartquote",
			s = "Sentence including punctuation",
			S = "Sentence with punctuation including whitespace",
			z = "Fold",
			t = "Tag",
		}
		local a = vim.deepcopy(i)
		for k, v in pairs(a) do
			a[k] = v:gsub(" including.*", "")
		end

		local ic = vim.deepcopy(i)
		local ac = vim.deepcopy(a)
		for key, name in pairs({ n = "Next", N = "Last" }) do
			i[key] = vim.tbl_extend("force", { name = "Inside " .. name .. " textobject" }, ic)
			a[key] = vim.tbl_extend("force", { name = "Around " .. name .. " textobject" }, ac)
		end
		require("which-key").register({
			mode = { "o", "x" },
			i = i,
			a = a,
		})
		local spec_pair = require("mini.ai").gen_spec.pair
		require("dotfiles.utils.mini").configure_mini_module("ai", {
			custom_textobjects = {
				["*"] = spec_pair("*", "*", { type = "greedy" }), -- Grab all asterisks when selecting
				["_"] = spec_pair("_", "_", { type = "greedy" }), -- Grab all underscores when selecting
				["l"] = { "%b[]%b()", "^%[().-()%]%([^)]+%)$" }, -- Link targeting name
				["L"] = { "%b[]%b()", "^%[.-%]%(()[^)]+()%)$" }, -- Link targeting href
			},
		}, {
			filetype = "markdown",
		})
		require("dotfiles.utils.mini").configure_mini_module("ai", {
			custom_textobjects = {
				["s"] = spec_pair("[[", "]]"),
			},
		}, {
			filetype = "lua",
		})

		-- ga/gA for align:
		require("mini.align").setup({})

		-- Paired commands such as [q/]q
		require("mini.bracketed").setup({})
		vim.keymap.set("n", "[t", "<cmd>tabprevious<cr>", {})
		vim.keymap.set("n", "]t", "<cmd>tabnext<cr>", {})
		vim.keymap.set("n", "[T", "<cmd>tabfirst<cr>", {})
		vim.keymap.set("n", "]T", "<cmd>tablast<cr>", {})

		-- :Bd[!] for layout-safe bufdelete
		require("mini.bufremove").setup()
		vim.api.nvim_create_user_command("Bd", function(args)
			MiniBufremove.delete(0, not args.bang)
		end, {
			bang = true,
		})

		-- gc for Comments
		require("mini.comment").setup()

		-- <leader>fm / <leader>fM for file manager
		require("mini.files").setup({
			windows = {
				preview = true,
			},
		})
		local show_dotfiles = true
		local filter_show = function()
			return true
		end
		local filter_hide = function(fs_entry)
			return not vim.startswith(fs_entry.name, ".")
		end
		local toggle_dotfiles = function()
			show_dotfiles = not show_dotfiles
			local new_filter = show_dotfiles and filter_show or filter_hide
			MiniFiles.refresh({ content = { filter = new_filter } })
		end
		vim.api.nvim_create_autocmd("User", {
			pattern = "MiniFilesBufferCreate",
			callback = function(args)
				local buf_id = args.data.buf_id
				-- Tweak left-hand side of mapping to your liking
				vim.keymap.set("n", "g.", toggle_dotfiles, { buffer = buf_id })
			end,
		})
		vim.keymap.set("n", "<leader>fm", "<cmd>lua MiniFiles.open(vim.api.nvim_buf_get_name(0), true)<cr>", {
			desc = "Open mini.files (directory of current file)",
		})
		vim.keymap.set("n", "<leader>fM", "<cmd>lua MiniFiles.open(vim.loop.cwd(), true)<cr>", {
			desc = "Open mini.files (cwd)",
		})

		-- nvim-colorizer configuration was:
		-- *: RGB, RRGGBB, RRGGBBAA, !names, !css
		-- !help, !lazy (off)
		-- html: names, !RRGGBBAA
		-- css: css, names, !RRGGBBAA
		-- scss/sass: css, names, !RRGGBBAA, sass
		-- TODO: css, sass, RRGGBBAA parsers
		-- TODO: disable parsers
		-- Hex Colors:
		require("mini.hipatterns").setup({
			highlighters = {
				short_hex_color = {
					pattern = "#%x%x%x%f[%X]",
					group = function(_, match)
						local r, g, b = match:sub(2, 2), match:sub(3, 3), match:sub(4, 4)
						local hex = string.format("#%s%s%s%s%s%s", r, r, g, g, b, b)
						return require("mini.hipatterns").compute_hex_color_group(hex, "bg")
					end,
				},
				hex_color = require("mini.hipatterns").gen_highlighter.hex_color(),
			},
		})
		require("dotfiles.utils.mini").disable_mini_module("hipatterns", {
			filetypes = { "help", "lazy", "markdown", "text" },
			buftype = { "quickfix" },
			terminal = true,
		})
		-- HTML Words:
		local names = vim.tbl_map(function(x)
			return string.lower(x)
		end, vim.tbl_keys(vim.api.nvim_get_color_map()))
		local cache = {}
		local function html_words(_, match)
			match = string.lower(match)
			if not vim.tbl_contains(names, match, {}) then
				return nil
			end
			if not cache[match] then
				cache[match] = "#" .. require("bit").tohex(vim.api.nvim_get_color_by_name(match), 6)
			end
			local color = cache[match]
			return require("mini.hipatterns").compute_hex_color_group(color, "bg")
		end
		-- Support short hex in
		require("dotfiles.utils.mini").configure_mini_module("hipatterns", {
			highlighters = {
				word = { pattern = "%w+", group = html_words },
			},
		}, {
			filetype = { "css", "html", "sass", "scss" },
		})

		-- Show current indentation context:
		require("mini.indentscope").setup({
			symbol = "▏",
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
		})
		require("dotfiles.utils.mini").disable_mini_module("indentscope", {
			terminal = true,
			filetype = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" },
			buftype = { "quickfix" },
		})

		-- Move lines with alt + hjkl:
		require("mini.move").setup()

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

		-- Useful text operators
		-- g= for calcuation; gx for exchanging regions (use twice to select and replace); gm multiply; gr for replace with register; gs for sorting
		require("mini.operators").setup()

		require("mini.pick").setup({
			options = {
				cache = true,
			},
			mappings = {
				mark = "<C-D>",
				mark_and_move = {
					char = "<C-X>",
					func = function()
						local mappings = MiniPick.get_picker_opts().mappings
						local keys = mappings.mark .. mappings.move_down
						vim.api.nvim_input(vim.api.nvim_replace_termcodes(keys, true, true, true))
					end,
				},
			},
		})
		vim.ui.select = MiniPick.ui_select
		local function open_multiple_files(results)
			for _, filepath in ipairs(results) do
				-- not the same as vim.fn.bufadd!
				vim.cmd.badd({ args = { filepath } })
			end

			-- switch to newly loaded buffers if on an empty buffer
			if vim.fn.bufname() == "" and not vim.bo.modified then
				vim.cmd.bwipeout()
				vim.cmd.buffer(results[1])
			end
		end
		local function open_files(item)
			local results = MiniPick.get_picker_matches().marked
			if #results > 1 then
				open_multiple_files(results)
				return
			end
			MiniPick.default_choose(item)
		end
		MiniPick.registry.buffers = function(local_opts, opts)
			local_opts = vim.tbl_deep_extend(
				"force",
				{ sort_lastused = false, sort_mru = false, include_current = true, include_unlisted = false },
				local_opts or {}
			)
			local buffers_output = vim.api.nvim_exec2(
				"buffers" .. (local_opts.include_unlisted and "!" or ""),
				{ output = true }
			).output
			local cur_buf_id, include_current = vim.api.nvim_get_current_buf(), local_opts.include_current
			local items = {}
			local default_selection_idx = 1
			for _, l in ipairs(vim.split(buffers_output, "\n")) do
				local buf_str, name = l:match("^%s*%d+"), l:match('"(.*)"')
				local buf_id = tonumber(buf_str)
				local flag = buf_id == vim.fn.bufnr("") and "%" or (buf_id == vim.fn.bufnr("#") and "#" or " ")
				local item = { text = name, bufnr = buf_id, flag = flag }
				if buf_id ~= cur_buf_id or include_current then
					if local_opts.sort_lastused and not local_opts.ignore_current_buffer and flag == "#" then
						default_selection_idx = 2
					end
					if local_opts.sort_lastused and (flag == "#" or flag == "%") then
						local idx = ((items[1] ~= nil and items[1].flag == "%") and 2 or 1)
						table.insert(items, idx, item)
					else
						table.insert(items, item)
					end
				end
			end
			if local_opts.sort_mru then
				table.sort(items, function(a, b)
					return vim.fn.getbufinfo(a.bufnr)[1].lastused > vim.fn.getbufinfo(b.bufnr)[1].lastused
				end)
			end

			local show = function(buf_id, items, query)
				MiniPick.default_show(buf_id, items, query, { show_icons = true })
			end
			local default_opts = { source = { name = "Buffers", show = show } }
			opts = vim.tbl_deep_extend("force", default_opts, opts or {}, { source = { items = items } })
			if default_selection_idx > 1 then
				vim.api.nvim_create_autocmd("User", {
					pattern = "MiniPickStart",
					once = true,
					callback = function()
						local mappings = MiniPick.get_picker_opts().mappings
						local keys = vim.fn["repeat"](mappings.move_down, default_selection_idx - 1)
						vim.api.nvim_input(vim.api.nvim_replace_termcodes(keys, true, true, true))
					end,
				})
			end
			return MiniPick.start(opts)
		end
		vim.keymap.set("n", "<C-H>", "<cmd>Pick help<cr>", { desc = "Help Tags" })
		vim.keymap.set("n", "<C-P>", function()
			MiniPick.builtin.files({}, { source = { choose = open_files, choose_marked = open_multiple_files } })
		end, { desc = "Files" })
		vim.keymap.set("n", "<leader>a", "<cmd>Pick buffers sort_lastused=true<cr>", { desc = "Buffers" })
		vim.keymap.set(
			{ "n", "v" },
			"<leader>*",
			"<cmd>Pick grep pattern='<cword>'<cr>",
			{ desc = "Search current word" }
		)

		-- use gS to split and join items in a list:
		require("mini.splitjoin").setup()

		local DotfilesStatusline = require("dotfiles.statusline")
		-- Override function used to make statusline:
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
		require("mini.statusline").setup({
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
					local org_headline = DotfilesStatusline.section_orgheadline()
					local wordcount = DotfilesStatusline.section_wordcount()

					return require("mini.statusline").combine_groups({
						"%<", -- Mark general truncate point
						{ hl = mode_hl, strings = { icon } },
						{ hl = mode_hl, strings = { " ", filename, " " } },
						{ hl = "MiniStatuslineLuaSnip", strings = { luasnip } },
						{ hl = "MiniStatuslineMacro", strings = { macro } },
						{ hl = "Statusline", strings = { "%=" } }, -- End left alignment
						{ strings = { org_headline } },
						{ hl = "MiniStatuslineWordcount", strings = { wordcount } },
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
		})

		-- Use cs/ys/ds to manipulate surrounding delimiters:
		require("mini.surround").setup({
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
		})
		-- Remap adding surrounding to Visual mode selection
		vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
		-- Make special mapping for "add surrounding for line"
		vim.keymap.set("n", "yss", "ys_", { noremap = false })
		require("dotfiles.utils.mini").configure_mini_module("surround", {
			custom_surroundings = {
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
		}, {
			filetype = "markdown",
		})
		require("dotfiles.utils.mini").configure_mini_module("surround", {
			custom_surroundings = {
				s = {
					input = { "%[%[().-()%]%]" },
					output = { left = "[[", right = "]]" },
				},
			},
		}, {
			filetype = "lua",
		})
		require("dotfiles.utils.mini").configure_mini_module("surround", {
			custom_surroundings = {
				l = {
					input = { "%[%[().-()%]%]" },
					output = { left = "[[", right = "]]" },
				},
			},
		}, {
			filetype = "org",
		})

		require("mini.tabline").setup({
			set_vim_settings = false,
			tabpage_section = "none",
		})
	end,
	dependencies = {
		{ "preservim/vim-textobj-sentence" }, -- Sentence object
	},
}
