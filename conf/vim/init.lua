-- Shims for neovim 0.9.5: {{{
if type(vim.fs.joinpath) ~= "function" then
	vim.fs.joinpath = function(...)
		return (table.concat({ ... }, "/"):gsub("//+", "/"))
	end
end
if type(vim.lsp.get_clients) ~= "function" then
	vim.lsp.get_clients = vim.lsp.get_active_clients
end
-- }}}
-- Custom Path {{{
vim.opt.runtimepath:append(vim.fn.expand("~/dotfiles/conf/vim"))
vim.opt.runtimepath:append(vim.fn.expand("~/dotfiles/conf/vim/after"))

vim.g.seadrive_path = vim.fn.has("mac") == 1
		and "~/Library/CloudStorage/SeaDrive-oncomouse(seafile.jetbear.us)/My Libraries"
	or "~/SeaDrive/My Libraries"
-- }}}
-- Basic Settings {{{
vim.opt.lazyredraw = true -- Don't redraw between macro runs (may make terminal flicker)

-- Line Numbering:
vim.opt.relativenumber = true

-- Folds:
vim.opt.foldlevel = 99
vim.opt.foldmethod = "indent"

-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Use split for search/replace preview:
vim.opt.inccommand = "split"

-- Height Of The Preview Window:
vim.opt.previewheight = 14

-- <C-z> expands wildcards in command mode
vim.opt.wildcharm = vim.api.nvim_replace_termcodes("<C-z>", true, true, true):byte()
vim.opt.wildignore = "*.o,*.obj,*~" -- stuff to ignore when tab completing
vim.opt.wildignore:append("*vim/backups*")
vim.opt.wildignore:append("*sass-cache*")
vim.opt.wildignore:append("*DS_Store*")
vim.opt.wildignore:append("vendor/rails/**")
vim.opt.wildignore:append("vendor/cache/**")
vim.opt.wildignore:append("node_modules/**")
vim.opt.wildignore:append("*.gem")
vim.opt.wildignore:append("log/**")
vim.opt.wildignore:append("tmp/**")
vim.opt.wildignore:append("*.png,*.jpg,*.gif")

-- Set path to current file direction and pwd:
vim.opt.path = ".,,"

-- Use better grep, if available:
if vim.fn.executable("rg") == 1 then
	vim.opt.grepprg = "rg --vimgrep --smart-case"
	vim.opt.grepformat = "%f:%l:%c:%m"
elseif vim.fn.executable("ag") == 1 then
	vim.opt.grepprg = "ag --vimgrep"
	vim.opt.grepformat = "%f:%l:%c:%m"
else
	vim.opt.grepprg = "grep -rn"
end

-- Linewrap:
vim.opt.wrap = true
vim.opt.showbreak = "↳ " -- Show a line has wrapped

vim.opt.dictionary = "/usr/share/dict/words"

-- Minimal Statusbar:
vim.opt.statusline = " %0.45f%m%h%w%r%= %y %l:%c "

-- Clipboard:
if vim.fn.has("clipboard") == 1 then
	vim.opt.clipboard = "unnamed"
	if vim.fn.has("unnamedplus") == 1 then
		vim.opt.clipboard:prepend("unnamedplus")
	end
end

-- Cmdheight=0 options:
vim.opt.cmdheight = 1
if vim.fn.has("nvim-0.9") == 1 and vim.opt.cmdheight == 0 then
	vim.opt.showcmdloc = "statusline"
end
vim.opt.showmode = false

-- Enable termguicolors by default
vim.opt.termguicolors = true

-- Enable experimental loader
if type(vim.loader) == "table" then
	vim.loader.enable()
end

-- }}}
-- Autogroups: {{{
local augroup = vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- }}}
-- Load mini.nvim + configure mini.deps {{{
local path_package = vim.fn.stdpath("data") .. "/mini.nvim/"
vim.opt.packpath:append(path_package)
vim.cmd("packloadall!")

local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
	vim.cmd('echo "Installing `mini.nvim`" | redraw')
	local clone_cmd = {
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/echasnovski/mini.nvim",
		mini_path,
	}
	vim.fn.system(clone_cmd)
	vim.cmd("packadd mini.nvim | helptags ALL")
	vim.cmd('echo "Installed `mini.nvim`" | redraw')
end

-- Set up 'mini.deps' (customize to your liking)
require("mini.deps").setup({ path = { package = path_package } })
-- }}}
-- Mac NeoVim Settings {{{
if vim.fn.has("mac") == 1 and vim.fn.has("nvim") == 1 then
	vim.g.python_host_prog = "/usr/bin/python2.7"
	vim.g.python3_host_prog = "/usr/local/bin/python3"
	vim.g.ruby_host_prog = vim.fn.expand("~/.asdf/shims/neovim-ruby-host")
	vim.g.node_host_prog = "/usr/local/lib/node_modules/neovim/bin/cli.js"
	--- This is macOS only, I believe, but it fixes slow start-up for clipboard:
	vim.g.clipboard = {
		copy = { ["+"] = "pbcopy", ["*"] = "pbcopy" },
		paste = { ["+"] = "pbpaste", ["*"] = "pbpaste" },
		name = "pbcopy",
		cache_enabled = 0,
	}
end
-- }}}
-- Tabs {{{
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false
-- }}}
-- Functions {{{
-- Open or close quickfix or loclist
local function list_toggle(pfx, force_open)
	local status
	if pfx == "c" then
		status = vim.fn.getqflist({ winid = 0 }).winid ~= 0
	else
		status = vim.fn.getloclist(0, { winid = 0 }).winid ~= 0
	end
	if not force_open then
		if status then
			vim.cmd(pfx .. "close")
			return
		end
		if pfx == "l" and #vim.fn.getloclist(0) == 0 then
			vim.cmd([[echohl ErrorMsg
			echo 'Location List is Empty.'
			echohl NONE]])
			return
		end
	end
	vim.cmd(pfx .. "open")
end

-- Run grep! unless we're in quickfix results, then run cfilter
local function grep_or_qfgrep()
	if vim.opt.buftype:get() == "quickfix" then
		-- Load cfilter in quickfix view:
		vim.cmd([[packadd cfilter]])
		local input = vim.fn.input("QFGrep/")
		if #input > 0 then
			local prefix = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "L" or "C"
			vim.cmd(prefix .. "filter /" .. input .. "/")
		end
	else
		local input = vim.fn.input("Grep/")
		if #input > 0 then
			vim.cmd('silent! grep! "' .. input .. '"')
		end
	end
end
-- }}}
-- Autocommands {{{
-- Line Number Colors in default:
vim.api.nvim_create_autocmd("ColorScheme", { group = augroup, pattern = "default", command = "hi LineNr ctermfg=7" })
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = augroup, pattern = "default", command = "hi LineNrAbove ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = augroup, pattern = "default", command = "hi LineNrBelow ctermfg=7" }
)
vim.api.nvim_create_autocmd("ColorScheme", {
	group = augroup,
	pattern = "default",
	command = "hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE gui=NONE",
})
vim.api.nvim_create_autocmd("ColorScheme", {
	group = augroup,
	pattern = "default",
	command = "hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE gui=NONE",
})

-- Turn Off Line Numbering:
vim.api.nvim_create_autocmd("TermOpen", { group = augroup, command = "setlocal nonumber norelativenumber" })

-- Close terminals when finished:
vim.api.nvim_create_autocmd("TermClose", {
	group = augroup,
	command = "bdelete",
})

-- Start QuickFix:
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = augroup,
	pattern = "[^l]*",
	callback = function()
		list_toggle("c", 1)
	end,
})
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = augroup,
	pattern = "l*",
	callback = function()
		list_toggle("l", 1)
	end,
})

-- Close Preview Window:
vim.api.nvim_create_autocmd("CompleteDone", {
	group = augroup,
	callback = function()
		if vim.fn.pumvisible() == 0 then
			vim.cmd("pclose")
		end
	end,
})
--}}}
-- Commands {{{

-- Formatting:
vim.api.nvim_create_user_command("Format", function(args)
	local buf = vim.api.nvim_get_current_buf()
	local ft = vim.opt_local.filetype:get()
	local has_formatter, config = pcall(require, "formatter.config")
	if has_formatter and config.values.filetype[ft] ~= nil then
		require("formatter.format").format(args.args, args.mod, args.line1, args.line2)
	elseif vim.b.dotfiles_lsp_can_format then
		vim.lsp.buf.format({
			bufnr = buf,
		})
	else
		vim.api.nvim_feedkeys("mxgggqG`x", "x", true)
	end
end, {
	desc = "Formatting with formatter.nvim, lsp, fallback",
	force = true,
	range = "%",
	nargs = "?",
})

-- Adjust Spacing:
vim.api.nvim_create_user_command("Spaces", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = true
	vim.opt_local.tabstop = tonumber(args.args)
	vim.opt_local.softtabstop = tonumber(args.args)
	vim.opt_local.shiftwidth = tonumber(args.args)
	vim.cmd("silent execute '%!expand -it" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})
vim.api.nvim_create_user_command("Tabs", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = false
	vim.opt_local.tabstop = tonumber(args.args)
	vim.opt_local.softtabstop = tonumber(args.args)
	vim.opt_local.shiftwidth = tonumber(args.args)
	vim.cmd("silent execute '%!unexpand -t" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})

vim.api.nvim_create_user_command("LazyGit", ":terminal lazygit<cr>", {
	force = true,
})
-- }}}
-- Keymaps {{{
-- Navigation in insert mode:
vim.keymap.set("i", "<C-a>", function()
	local sc = vim.fn.col(".")
	vim.cmd("normal! ^")
	if vim.fn.col(".") == sc then
		vim.cmd("normal! 0")
	end
end, { silent = true, desc = "Move to start of line" })
vim.keymap.set("i", "<C-e>", "<End>", { silent = true, desc = "Move to end of line" })
vim.keymap.set("i", "<C-b>", "<Left>", { desc = "Move back one character" })
vim.keymap.set("i", "<C-f>", "<Right>", { desc = "Move forward one character" })
local function move_word(backwards)
	return function()
		local _, new_position =
			unpack(vim.fn.searchpos(backwards and [[\<]] or [[\>]], backwards and "bn" or "n", vim.fn.line(".")))
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if new_position == 0 then
			col = backwards and 0 or #vim.api.nvim_get_current_line()
		else
			col = new_position - 1
		end
		vim.api.nvim_win_set_cursor(0, { row, col })
	end
end
vim.keymap.set("i", "<A-b>", move_word(true), { desc = "Move back one word" })
vim.keymap.set("i", "<A-f>", move_word(), { desc = "Move forward one word" })

-- More emacs mappings:
vim.keymap.set("i", "<C-d>", "<C-o>x", { desc = "Delete character in front of mark." })
vim.keymap.set("i", "<M-H>", "<C-o><<", { desc = "Decrease indent." })
vim.keymap.set("i", "<M-L>", "<C-o>>>", { desc = "Decrease indent." })
vim.keymap.set("i", "", "<C-o>u", { desc = "Undo." })
vim.keymap.set("i", "", "<C-o>d0", { desc = "Kill line backwards." })
vim.keymap.set("i", "<M-BS>", "<C-o>db<C-o>i", { desc = "Kill word backwards." })

-- Clear Currently Highlighted Regexp:
vim.keymap.set(
	"n",
	"<leader>cr",
	':let<C-u>let @/=""<CR>',
	{ silent = true, noremap = true, desc = "Clear current regexp" }
)

-- Tab navigation:
vim.keymap.set("n", "]t", "<cmd>tabnext<CR>", { silent = true, noremap = true, desc = "Jump to next tab" })
vim.keymap.set("n", "[t", "<cmd>tabprev<CR>", { silent = true, noremap = true, desc = "Jump to previous tab" })

-- Toggle Quickfix:
vim.keymap.set("n", "<leader>q", function()
	list_toggle("c")
end, { silent = true, noremap = true, desc = "Display quickfix list" })
vim.keymap.set("n", "<leader>d", function()
	list_toggle("l")
end, { silent = true, noremap = true, desc = "Display location list" })

-- Project Grep:
vim.keymap.set("n", "<leader>/", function()
	grep_or_qfgrep()
end, { silent = true, noremap = true, desc = "Search in current project using grep()" })

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", function()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end, { silent = true, noremap = true })

-- Calculator:
vim.keymap.set(
	"i",
	"<C-X><C-A>",
	"<C-O>yiW<End>=<C-R>=<C-R>0<CR>",
	{ silent = true, noremap = true, desc = "Calculate" }
)

-- Vertical split like in my Tmux config:
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>", { desc = "Split vertically" })

-- Jump to last buffer:
vim.keymap.set("n", "<leader><leader>", "<cmd>b#<cr>", { desc = "Jump to last buffer" })

vim.keymap.set("i", "<C-X><C-S>", "<c-o>:silent! w<cr>", { desc = "Save current buffer", silent = true })

-- LazyGit:
vim.keymap.set("n", "<leader>lg", "<cmd>LazyGit<cr>", { noremap = true, silent = true, desc = "Lazygit" })
-- }}}
-- Plugins {{{
require("rocks") -- Add luarocks to the path
require("select-digraphs").setup({}) -- Configure select-digraphs

-- Core Plugins: {{{
MiniDeps.add("nvim-lua/plenary.nvim")
MiniDeps.add("oncomouse/lspize.nvim")
MiniDeps.add("kyazdani42/nvim-web-devicons")
MiniDeps.add("ku1ik/vim-pasta") -- fix block paste for Neovim

vim.g.sleuth_org_heuristics = false -- Disable sleuthing on org files
MiniDeps.add("tpope/vim-sleuth") -- Automatically set indent
vim.g["asterisk#keeppos"] = 1
MiniDeps.add("haya14busa/vim-asterisk") -- Fancy * and # bindings
MiniDeps.later(function()
	vim.keymap.set("", "*", "<Plug>(asterisk-z*)", {
		desc = "Search forward for the [count]'th occurrence of the word nearest to the cursor",
	})
	vim.keymap.set("", "#", "<Plug>(asterisk-z#)", {
		desc = "Search backward for the [count]'th occurrence of the word nearest to the cursor",
	})
	vim.keymap.set("", "g*", "<Plug>(asterisk-gz*)", {
		desc = "Search forward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
	})
	vim.keymap.set("", "g#", "<Plug>(asterisk-gz#)", {
		desc = "Search backward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
	})
end)
MiniDeps.add("oncomouse/markdown.nvim")
-- }}}
-- Theme: {{{
MiniDeps.add({
	source = "catppuccin/nvim",
	hooks = {
		post_checkout = function()
			vim.cmd("CatppuccinBuild")
		end,
	},
})

MiniDeps.now(function()
	require("catppuccin").setup({
		flavour = "latte",
		transparent_background = true,
		integrations = {
			mini = true,
			notify = true,
		},
		custom_highlights = function(colors)
			return {
				Folded = {
					fg = colors.subtext0,
					bg = colors.surface0,
				},
				MiniStatuslineFileinfo = {
					fg = colors.surface2,
					bg = colors.base,
				},
				MiniStatuslineModeNormal = {
					bg = colors.subtext0,
					fg = colors.base,
					style = {},
				},
				MiniStatuslineModeInsert = {
					bg = colors.green,
					fg = colors.base,
					style = {},
				},
				MiniStatuslineModeVisual = {
					bg = colors.sapphire,
					fg = colors.base,
					style = {},
				},
				MiniStatuslineModeReplace = { style = {} },
				MiniStatuslineModeCommand = { style = {} },
				MiniStatuslineModeOther = {
					bg = colors.mauve,
					fg = colors.base,
					style = {},
				},
				MiniStatuslineLocationRow = {
					fg = colors.mauve,
				},
				MiniStatuslineLocationColumn = {
					fg = colors.sapphire,
				},
				MiniStatuslineLocationPercentage = {
					fg = colors.blue,
				},
				MiniStatuslineDiagnosticError = {
					bg = colors.red,
					fg = colors.base,
				},
				MiniStatuslineDiagnosticWarn = {
					bg = colors.yellow,
					fg = colors.base,
				},
				MiniStatuslineDiagnosticInfo = {
					bg = colors.blue,
					fg = colors.base,
				},
				MiniStatuslineDiagnosticHint = {
					bg = colors.rosewater,
					fg = colors.base,
				},
				MiniStatuslineMacro = {
					bg = colors.flamingo,
					fg = colors.surface0,
				},
				MiniStatuslineLuaSnip = {
					bg = colors.sky,
					fg = colors.surface0,
				},
				MiniStatuslineWordcount = {
					fg = colors.yellow,
				},
				MiniTablineCurrent = {
					fg = colors.subtext0,
					style = {},
				},
				MiniTablineVisible = {
					fg = colors.surface1,
				},
				MiniTablineHidden = {
					fg = colors.surface1,
				},
				MiniTablineModifiedCurrent = {
					fg = colors.subtext0,
					style = {
						"bold",
					},
				},
				MiniTablineModifiedVisible = {
					fg = colors.surface1,
					style = {
						"bold",
					},
				},
				MiniTablineModifiedHidden = {
					fg = colors.surface1,
					style = {
						"bold",
					},
				},
				NotifyBackground = {
					bg = colors.base,
				},
				gitCommitOverflow = { fg = colors.red },
				gitCommitSummary = { fg = colors.green },
			}
		end,
	})

	vim.cmd("colorscheme catppuccin")
end)

MiniDeps.later(function()
	vim.api.nvim_create_user_command("CatppuccinBuild", function()
		vim.notify("Updating catppuccin cterm information.", vim.log.levels.INFO, {
			title = "catppuccin-cterm.nvim",
		})
		for name, _ in pairs(require("catppuccin").flavours) do
			local colorscheme_name = string.format("catppuccin-%s", name)
			require("mini.colors").get_colorscheme(colorscheme_name):add_cterm_attributes():write({
				name = colorscheme_name,
			})
		end
	end, {
		force = true,
	})
end)
-- }}}
-- mini.nvim {{{
MiniDeps.later(function()
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
				local last_line_length = #vim.fn.getline("$")
				local to = {
					line = vim.fn.line("$"),
					col = last_line_length == 0 and 1 or last_line_length,
				}
				return { from = from, to = to, vis_mode = "V" }
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
		group = augroup,
		pattern = vim.fn.join(vim.tbl_keys(custom_textobjects), ","),
		callback = function()
			local ft = vim.opt.filetype:get()
			vim.b.miniai_config = {
				custom_textobjects = custom_textobjects[ft],
			}
		end,
	})
end)

MiniDeps.later(function()
	-- ga and gA for alignment:
	require("mini.align").setup({})
end)

MiniDeps.now(function()
	-- mini.basics:
	require("mini.basics").setup({
		options = {
			basic = true,
		},
		mappings = {
			move_with_alt = true,
			windows = true,
		},
	})
	vim.opt.completeopt:append("preview")
	vim.opt.shortmess:append("Wc")
end)

MiniDeps.later(function()
	vim.keymap.set("n", "<M-Up>", "<cmd>lua MiniMove.move_line('up')<CR>")
	vim.keymap.set("n", "<M-Down>", "<cmd>lua MiniMove.move_line('down')<CR>")
end)

MiniDeps.later(function()
	-- mini.bracketed:
	require("mini.bracketed").setup({})
	vim.keymap.set("n", "[t", "<cmd>tabprevious<cr>", {})
	vim.keymap.set("n", "]t", "<cmd>tabnext<cr>", {})
	vim.keymap.set("n", "[T", "<cmd>tabfirst<cr>", {})
	vim.keymap.set("n", "]T", "<cmd>tablast<cr>", {})
end)

MiniDeps.later(function()
	-- :Bd[!] for layout-safe bufdelete
	require("mini.bufremove").setup({})
	vim.api.nvim_create_user_command("Bd", function(args)
		require("mini.bufremove").delete(0, not args.bang)
	end, {
		bang = true,
	})
end)

MiniDeps.later(function()
	-- Use mini.clue for assisting with keybindings:
	require("mini.clue").setup({
		window = {
			config = {
				anchor = "SW",
				width = math.floor(0.618 * vim.o.columns),
				row = "auto",
				col = "auto",
			},
		},
		triggers = {
			-- Leader triggers
			{ mode = "n", keys = "<Leader>" },
			{ mode = "x", keys = "<Leader>" },

			-- Built-in completion
			{ mode = "i", keys = "<C-x>" },

			-- `g` key
			{ mode = "n", keys = "g" },
			{ mode = "x", keys = "g" },

			-- Marks
			{ mode = "n", keys = "'" },
			{ mode = "n", keys = "`" },
			{ mode = "x", keys = "'" },
			{ mode = "x", keys = "`" },

			-- Registers
			{ mode = "n", keys = '"' },
			{ mode = "x", keys = '"' },
			{ mode = "i", keys = "<C-r>" },
			{ mode = "c", keys = "<C-r>" },

			-- Window commands
			{ mode = "n", keys = "<C-w>" },

			-- `z` key
			{ mode = "n", keys = "z" },
			{ mode = "x", keys = "z" },
		},
		clues = {
			-- Enhance this by adding descriptions for <Leader> mapping groups
			require("mini.clue").gen_clues.builtin_completion(),
			require("mini.clue").gen_clues.g(),
			require("mini.clue").gen_clues.marks(),
			require("mini.clue").gen_clues.registers(),
			require("mini.clue").gen_clues.windows(),
			require("mini.clue").gen_clues.z(),
		},
	})
end)

MiniDeps.later(function()
	-- gc for commenting/uncommenting:
	require("mini.comment").setup({})
end)

MiniDeps.later(function()
	require("mini.extra").setup({})
end)

MiniDeps.later(function()
	-- file browser <leader>fm / <leader>fM
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
	-- Set buffer specific maps in minifiles:
	vim.api.nvim_create_autocmd("User", {
		pattern = "MiniFilesBufferCreate",
		callback = function(args)
			local buf_id = args.data.buf_id
			vim.keymap.set("n", "g.", toggle_dotfiles, { buffer = buf_id })
			vim.keymap.set("n", "<C-P>", "<C-P>", { buffer = buf_id }) -- nmap <buffer> <C-P> <C-P>
			vim.keymap.set("n", "<Esc>", "<cmd>lua MiniFiles.close()<cr>", { buffer = buf_id })
		end,
	})
	vim.keymap.set("n", "<leader>fm", "<cmd>lua MiniFiles.open(vim.api.nvim_buf_get_name(0), true)<cr>", {
		desc = "Open mini.files (directory of current file)",
	})
	vim.keymap.set("n", "<leader>fM", "<cmd>lua MiniFiles.open(vim.loop.cwd(), true)<cr>", {
		desc = "Open mini.files (cwd)",
	})
end)

MiniDeps.later(function()
	-- Highlight patterns:
	local hipatterns = require("mini.hipatterns")
	hipatterns.setup({
		highlighters = {
			-- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
			fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
			hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
			todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
			note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },

			-- Highlight hex color strings (`#rrggbb`) using that color
			hex_color = hipatterns.gen_highlighter.hex_color(),
		},
	})
end)

MiniDeps.later(function()
	-- Indentscope:
	require("mini.indentscope").setup({
		symbol = "│",
		options = { try_as_border = true },
		draw = {
			animation = require("mini.indentscope").gen_animation.none(),
		},
	})
	-- Disable:
	vim.api.nvim_create_autocmd("FileType", {
		pattern = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" },
		callback = function()
			vim.b.miniindentscope_disable = true
		end,
	})
end)

MiniDeps.later(function()
	require("mini.notify").setup()
	vim.keymap.set("n", "<leader>nn", MiniNotify.show_history, { desc = "Help Tags" })
end)

MiniDeps.later(function()
	require("mini.operators").setup()
end)

MiniDeps.now(function()
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
end)

MiniDeps.later(function()
	require("mini.move").setup({})
end)

MiniDeps.later(function()
	require("mini.pick").setup({
		options = {
			cache = true,
		},
		mappings = {
			mark = "<C-D>",
			mark_and_move = {
				char = "<C-X>",
				func = function()
					local mappings = require("mini.pick").get_picker_opts().mappings
					local keys = mappings.mark .. mappings.move_down
					vim.api.nvim_input(vim.api.nvim_replace_termcodes(keys, true, true, true))
				end,
			},
		},
	})
	vim.ui.select = require("mini.pick").ui_select
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
		local results = require("mini.pick").get_picker_matches().marked
		if #results > 1 then
			open_multiple_files(results)
			return
		end
		require("mini.pick").default_choose(item)
	end
	require("mini.pick").registry.buffers = function(local_opts, opts)
		local_opts = vim.tbl_deep_extend(
			"force",
			{ sort_lastused = false, sort_mru = false, include_current = true, include_unlisted = false },
			local_opts or {}
		)
		local buffers_output = vim.api.nvim_exec("buffers" .. (local_opts.include_unlisted and "!" or ""), true)
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
			require("mini.pick").default_show(buf_id, items, query, { show_icons = true })
		end
		local default_opts = { source = { name = "Buffers", show = show } }
		opts = vim.tbl_deep_extend("force", default_opts, opts or {}, { source = { items = items } })
		if default_selection_idx > 1 then
			vim.api.nvim_create_autocmd("User", {
				pattern = "MiniPickStart",
				once = true,
				callback = function()
					local mappings = require("mini.pick").get_picker_opts().mappings
					local keys = vim.fn["repeat"](mappings.move_down, default_selection_idx - 1)
					vim.api.nvim_input(vim.api.nvim_replace_termcodes(keys, true, true, true))
				end,
			})
		end
		return require("mini.pick").start(opts)
	end

	vim.keymap.set("n", "<leader>hf", "<cmd>Pick help<cr>", { desc = "Search help tags" })
	vim.keymap.set("n", "<leader>pf", function()
		MiniPick.builtin.files({}, {
			mappings = {
				open_binary = {
					char = "<C-O>",
					func = function()
						local file_path = vim.fs.joinpath(
							MiniPick.get_picker_opts().source.cwd,
							MiniPick.get_picker_matches().current
						)
						vim.fn.system({
							"open",
							file_path,
						})
						MiniPick.stop()
					end,
				},
			},
			source = {
				choose = open_files,
				choose_marked = open_multiple_files,
			},
		})
	end, { desc = "Find files in project" })
	vim.keymap.set("n", "<leader>ff", "<cmd>Pick explorer<CR>", { desc = "Find file" })
	vim.keymap.set("n", "<leader>a", "<cmd>Pick buffers sort_lastused=true<cr>", { desc = "Buffers" })
	vim.keymap.set({ "n", "v" }, "<leader>*", "<cmd>Pick grep pattern='<cword>'<cr>", { desc = "Search current word" })
end)

MiniDeps.later(function()
	-- use gS to split and join items in a list:
	require("mini.splitjoin").setup({})
end)

MiniDeps.now(function()
	require("mini.statusline").setup({
		content = {
			inactive = function()
				return require("mini.statusline").combine_groups({
					{ hl = "StatuslineNC", strings = { "%t%m" } },
				})
			end,
		},
	})
end)

MiniDeps.later(function()
	-- Replace vim-surround:
	require("mini.surround").setup({
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
	})
	-- Remap adding surrounding to Visual mode selection
	vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
	-- Make special mapping for "add surrounding for line"
	vim.keymap.set("n", "yss", "ys_", { noremap = false })

	-- Per-file surroundings:
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
		group = augroup,
		pattern = vim.fn.join(vim.tbl_keys(custom_surroundings), ","),
		callback = function()
			local ft = vim.opt.filetype:get()
			vim.b.minisurround_config = {
				custom_surroundings = custom_surroundings[ft],
			}
		end,
	})
end)

MiniDeps.now(function()
	require("mini.tabline").setup({
		set_vim_settings = false,
		tabpage_section = "none",
	})
end)
-- }}}
-- dial.nvim {{{
MiniDeps.add("monaqa/dial.nvim")
MiniDeps.later(function()
	vim.keymap.set({ "n", "v" }, "<C-a>", "<Plug>(dial-increment)")
	vim.keymap.set({ "n", "v" }, "<C-x>", "<Plug>(dial-decrement)")
	vim.keymap.set({ "n", "v" }, "g<C-a>", "g<Plug>(dial-increment)")
	vim.keymap.set({ "n", "v" }, "g<C-x>", "g<Plug>(dial-decrement)")
	local augend = require("dial.augend")
	require("dial.config").augends:register_group({
		-- default augends used when no group name is specified
		default = {
			augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
			augend.integer.alias.hex, -- nonnegative hex number	 (0x01, 0x1a1f, etc.)
			augend.constant.alias.bool, -- boolean value (true <-> false)
			augend.date.alias["%Y/%m/%d"], -- date (2022/02/18, etc.)
			augend.date.alias["%m/%d/%Y"], -- date (02/19/2022)
			-- augend.date.alias["%m-%d-%Y"], -- date (02-19-2022)
			-- augend.date.alias["%Y-%m-%d"], -- date (02-19-2022)
			augend.date.new({
				pattern = "%m.%d.%Y",
				default_kind = "day",
				only_valid = true,
				word = false,
			}),
			augend.misc.alias.markdown_header,
		},
	})
end)
-- }}}
-- LSP: {{{
MiniDeps.add("neovim/nvim-lspconfig")
MiniDeps.add("folke/neodev.nvim")
-- }}}
-- Treesitter {{{
local nvim_treesitter = {
	installed_parser = {
		"bash",
		"bibtex",
		"c",
		"cmake",
		"comment",
		"cpp",
		"css",
		"diff",
		"dockerfile",
		"fennel",
		"fish",
		"go",
		"graphql",
		"html",
		"http",
		"java",
		"javascript",
		"jsdoc",
		"json",
		"jsonc",
		"latex",
		"lua",
		"luadoc",
		"luap",
		"make",
		"markdown",
		"markdown_inline",
		"ninja",
		"nix",
		"norg",
		"org",
		"perl",
		"php",
		"python",
		"query",
		"r",
		"rasi",
		"regex",
		"ruby",
		"rust",
		"scss",
		"svelte",
		"tsx",
		"typescript",
		"vim",
		"vimdoc",
		"vue",
		"xml",
		"yaml",
		"zig",
	},
	parser_configs = {

		-- TODO: Use repo in https://github.com/serenadeai/tree-sitter-scss/pull/19
		scss = {
			install_info = {
				url = "https://github.com/goncharov/tree-sitter-scss",
				files = { "src/parser.c", "src/scanner.c" },
				branch = "placeholders",
				revision = "30c9dc19d3292fa8d1134373f0f0abd8547076e8",
			},
			maintainers = { "@goncharov" },
		},
	},
	line_threshold = {
		base = {
			cpp = 30000,
			javascript = 30000,
			perl = 10000,
		},
		extension = {
			cpp = 10000,
			javascript = 3000,
			perl = 3000,
		},
	}, -- Disable check for highlight, highlight usage, highlight context module
}

nvim_treesitter.should_highlight_disable = function(lang, bufnr)
	local line_count = vim.api.nvim_buf_line_count(bufnr or 0)

	return nvim_treesitter.line_threshold[lang] ~= nil and line_count > nvim_treesitter.line_threshold[lang].base
end

nvim_treesitter.should_buffer_higlight_disable = function()
	local ft = vim.bo.ft
	local bufnr = vim.fn.bufnr()
	return nvim_treesitter.should_highlight_disable(ft, bufnr)
end

MiniDeps.add({
	source = "nvim-treesitter/nvim-treesitter",
	hooks = {
		post_checkout = function()
			vim.cmd("TSUpdate")
		end,
	},
	depends = {
		"andymass/vim-matchup",
	},
})
vim.g.matchup_matchparen_offscreen = {
	method = "popup",
}
MiniDeps.later(function()
	local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
	for f, c in pairs(nvim_treesitter.parser_configs) do
		parser_configs[f] = c
	end
	local ts_foldexpr_augroup_id = vim.api.nvim_create_augroup("nvim_treesitter_foldexpr", {})

	vim.api.nvim_create_autocmd("FileType", {
		pattern = vim.fn.join(nvim_treesitter.installed_parser, ","),
		group = ts_foldexpr_augroup_id,
		callback = function()
			vim.opt_local.foldexpr = "v:lua.vim.treesitter.foldexpr()"
			vim.opt_local.foldmethod = "expr"
		end,
		desc = "Set fold method for treesitter",
	})

	require("nvim-treesitter.configs").setup({
		ensure_installed = nvim_treesitter.installed_parser,
		highlight = {
			enable = true,
			-- additional_vim_regex_highlighting = false,
			additional_vim_regex_highlighting = { "org" },
			disable = nvim_treesitter.should_highlight_disable,
		},
		matchup = {
			enable = nvim_treesitter.should_buffer_higlight_disable,
		},
	})
end)

-- }}}
-- lexima.vim {{{
-- Lexima Settings:
vim.g.dotfiles_load_lexima = false
vim.g.lexima_map_escape = ""
vim.g.lexima_enable_space_rules = 0
vim.g.lexima_enable_endwise_rules = 1
vim.g.lexima_disable_closetag = 0
vim.g.lexima_no_default_rules = 1
-- Electric Quotes (https://www.gnu.org/software/emacs/manual/html_node/emacs/Quotation-Marks.html)
vim.api.nvim_create_user_command(
	"ToggleElectricQuotes",
	[[lua vim.b.use_electric_quotes = not (vim.b.use_electric_quotes or false)]],
	{}
)
local function load_lexima()
	-- Load default rules:
	vim.fn["lexima#set_default_rules"]()

	-- Utilities:
	local add_rule = vim.fn["lexima#add_rule"]

	local function make_markdown_bi_rule(char, escape)
		local esc_char = escape and [[\]] .. char or char
		add_rule({
			char = char,
			input_after = char,
			filetype = { "text", "markdown" },
			except = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
		}) -- Create italic pair
		add_rule({
			char = "<Space>",
			delete = 1,
			filetype = { "text", "markdown" },
			at = "^" .. esc_char .. [[\%#]] .. esc_char,
		}) -- Handle bulleted item
		add_rule({
			char = char,
			at = [[\%#]] .. esc_char,
			leave = char,
			filetype = { "text", "markdown" },
			except = esc_char .. [[\{1\}\%#]],
		}) -- Leave italic pair
		add_rule({
			char = char,
			at = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
			leave = 2,
			filetype = { "text", "markdown" },
		}) -- Leave bold pair
		add_rule({
			char = "<BS>",
			at = esc_char .. [[\%#]] .. esc_char,
			delete = char,
			filetype = { "text", "markdown" },
		}) -- Delete pair
	end
	-- Take a string or list of strings, produce a basic pair
	local function make_pair(chars, escape, opts)
		chars = type(chars) == "table" and chars or { chars }
		opts = opts or {}
		if type(escape) == "table" then
			opts = escape
			escape = false
		end
		for _, char in pairs(chars) do
			local esc_char = escape and [[\]] .. char or char
			add_rule(vim.tbl_extend("keep", {
				char = char,
				input_after = char,
			}, opts))
			add_rule(vim.tbl_extend("keep", {
				char = char,
				at = [[\%#]] .. esc_char,
				leave = 1,
			}, opts))
			add_rule(vim.tbl_extend("keep", {
				char = "<BS>",
				at = esc_char .. [[\%#]] .. esc_char,
				delete = 1,
			}, opts))
		end
	end

	-- Lexima Rules
	-- Correct unbalanced pairs:
	add_rule({
		char = "<BS>",
		at = [[""\%#"]],
		delete = 1,
		input = [[<BS><BS>"]],
		input_after = [["]],
	})
	add_rule({
		char = "<BS>",
		at = [[((\%#)]],
		delete = 1,
		input = [[<BS><BS>(]],
		input_after = [[)]],
	})
	add_rule({
		char = "<BS>",
		at = [[[^(](\%#))]],
		delete = 1,
	})

	-- Markdown rules:
	-- Links:
	add_rule({
		char = "]",
		at = [=[\[[^]]*\%#\]]=],
		except = [==[\[@[^]]*\%#[^]]*\]]==],
		leave = "]",
		input = "(",
		input_after = ")",
		filetype = { "text", "markdown" },
	})
	-- Blockquotes:
	add_rule({
		char = "<BS>",
		input = "<BS><BS>",
		at = [[^> \%#]],
		filetype = { "text", "markdown" },
	})
	add_rule({
		char = "<CR>",
		at = [[^> .\+\%#$]],
		input = "<CR>> ",
		filetype = { "text", "markdown" },
	})
	add_rule({
		char = "<CR>",
		at = [[^> \%#$]],
		input = "<BS><BS><CR>",
		filetype = { "text", "markdown" },
	})
	add_rule({
		char = ">",
		input = "> ",
		at = [[^\%#]],
		filetype = { "text", "markdown" },
	})
	-- Unordered Lists:
	add_rule({
		char = "<CR>",
		at = [[^\s*\([*-]\) .*\%#$]],
		filetype = { "text", "markdown" },
		with_submatch = true,
		input = [[<CR>\1 ]],
		except = [[^\s*\([*-]\) \%#$]],
	})
	add_rule({
		char = "<CR>",
		at = [[^\s*\([*-]\) \%#$]],
		filetype = { "text", "markdown" },
		input = [[<Home><C-O>"_D<CR>]],
	})
	add_rule({
		char = "<BS>",
		at = [[^\(\s*\)[*-] \%#$]],
		filetype = { "text", "markdown" },
		with_submatch = true,
		input = [[<Home><C-O>"_D\1]],
	})
	-- Ordered Lists (including automatic increment):
	add_rule({
		char = "<CR>",
		at = [[^\s*\([0-9]\+\)\..*\%#$]],
		filetype = { "text", "markdown", "org" },
		with_submatch = true,
		input = [[<CR>\1. <Home><C-o>:exec "normal! \<c-a\>" "$"<CR>]],
		except = [[^\s*\([0-9]\)\. \%#$]],
	})
	add_rule({
		char = "<CR>",
		at = [[^\s*\([0-9]\+\)\. \%#$]],
		filetype = { "text", "markdown", "org" },
		input = [[<Home><C-O>"_D<CR>]],
	})
	add_rule({
		char = "<BS>",
		at = [[^\(\s*\)[0-9]\+\. \%#$]],
		filetype = { "text", "markdown", "org" },
		with_submatch = true,
		input = [[<Home><C-O>"_D\1]],
	})
	-- Tasks:
	add_rule({
		char = "[",
		input = "[ ] ",
		at = [[^\s*[*-+] \s*\%#]],
		filetype = { "text", "markdown", "org" },
	})
	add_rule({
		char = "[",
		input = "[ ] ",
		at = [[^\s*\d\+\. \s*\%#]],
		filetype = { "text", "markdown", "org" },
	})
	add_rule({
		char = "<BS>",
		input = "<BS><BS><BS>",
		at = [[^\s*[*-+\d]\+\.\{0,1\} \s*\[.\]\%#]],
		filetype = { "text", "markdown", "org" },
	})
	-- Bold/Italic Pairs:
	make_markdown_bi_rule("*", true)
	make_markdown_bi_rule("_")

	-- Org-only rules:
	make_pair({ "_", "+" }, {
		filetype = "org",
	})
	make_pair({ "~", "*" }, true, {
		filetype = "org",
	})
	-- Don't pair asterisks for headlines:
	add_rule({
		char = "*",
		at = [[^\**\%#]],
		filetype = "org",
	})
	-- Also don't pair if we are actually starting a line with italics:
	add_rule({
		char = "*",
		at = [[^\*\+\w[^*]\+\%#]],
		filetype = "org",
	})
	-- Don't pair pluses in dates:
	add_rule({
		char = "+",
		at = [[<[^>]\+\%#]],
		filetype = "org",
	})
	-- Ordered List:
	add_rule({
		char = "+",
		input = "+ ",
		at = [[^\s*\%#]],
		filetype = "org",
	})
	add_rule({
		char = "<BS>",
		at = [[^\(\s*\)[+-] \%#$]],
		filetype = "org",
		with_submatch = true,
		input = [[<Home><C-O>"_D\1]],
	})
	add_rule({
		char = "<CR>",
		at = [[^\s*\([+-]\) .*\%#$]],
		filetype = "org",
		with_submatch = true,
		input = [[<CR>\1 ]],
		except = [[^\s*\([+-]\) \%#$]],
	})
	add_rule({
		char = "<CR>",
		at = [[^\s*\([+-]\) \%#$]],
		filetype = "org",
		input = [[<Home><C-O>"_D]],
	})
	-- Handle cookies properly:
	add_rule({
		char = "/",
		input = "/<Right>",
		at = [==[\[\%#\]]==],
		filetype = "org",
	})
	add_rule({
		char = "%",
		input = "%<Right>",
		at = [==[\[\%#\]]==],
		filetype = "org",
	})
	-- Links:
	add_rule({
		char = "[",
		at = [[^\s*[+-] \[ \] \%#]],
		input = "<BS><Left><BS>[",
		input_after = "]",
		filetype = "org",
	}) -- Convert a todo item into a link
	-- Links:
	add_rule({
		char = "[",
		at = [[^\s*[0-9]\.\s\+\[ \]\s\+\%#]],
		input = "<BS><Left><BS>[",
		input_after = "]",
		filetype = "org",
	}) -- Convert a todo item into a link
	add_rule({
		char = "]",
		at = [==[\%#\]]==],
		input = "<Right>",
		filetype = "org",
	}) -- Leave a square bracket
	add_rule({
		char = "/",
		at = [==[/\%#]==],
		input = "",
		input_after = "/",
		filetype = "org",
	}) -- Autocomplete italics from //
	add_rule({
		char = "<BS>",
		at = [==[/\%#/]==],
		input = "<BS>",
		delete = 1,
		filetype = "org",
	}) -- Autocomplete italics from //

	-- Rules for help files:
	add_rule({
		char = "<Bar>",
		input_after = "<Bar>",
		filetype = "help",
	})
	add_rule({
		char = "<Bar>",
		at = [[\%#|]],
		leave = 1,
		filetype = "help",
	})
	add_rule({
		char = "<BS>",
		at = [[|\%#|]],
		delete = 1,
		filetype = "help",
	})

	-- Electric Quotes
	add_rule({
		char = "`",
		input = "‘",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
		except = [[\%#`]],
	})
	add_rule({
		char = "`",
		at = [[\%#`]],
		delete = 1,
	})
	add_rule({
		char = "`",
		at = [[‘\%#]],
		input = "<BS>“",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "'",
		at = [[[‘“][^‘“]*\%#]],
		except = [[’\%#]],
		input = "’",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "'",
		at = [[’\%#]],
		input = "<BS>”",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		at = [[“\%#]],
		input = "<BS>‘",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		at = [[”\%#]],
		input = "<BS>’",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = ".",
		at = [[\.\.\%#]],
		input = "<BS><BS>…",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		at = [[…\%#]],
		input = "<BS>..",
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})

	-- Electric dashes and dots
	add_rule({
		char = "-",
		input = "<BS>–",
		at = [[-\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "-",
		input = "<BS>—",
		at = [[–\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		input = "<BS>–",
		at = [[—\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		input = "<BS>-",
		at = [[–\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = ".",
		input = "<BS><BS>…",
		at = [[..\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})
	add_rule({
		char = "<BS>",
		input = "<BS>..",
		at = [[…\%#]],
		enabled = function()
			return vim.b.use_electric_quotes
		end,
	})

	-- XML-style closetag:
	if vim.g.lexima_disable_closetag == 0 then
		add_rule({
			char = "<",
			input_after = ">",
			filetype = { "html", "xml", "javascript", "javascriptreact" },
		})
		add_rule({
			char = "<BS>",
			at = [[<\%#>]],
			delete = 1,
			filetype = { "html", "xml", "javascript", "javascriptreact" },
		})
		add_rule({
			char = ">",
			at = [[<\(\w\+\)[^>]*\%#>]],
			leave = 1,
			input_after = [[</\1>]],
			with_submatch = 1,
			filetype = { "html", "xml", "javascript", "javascriptreact" },
		})
	end
end

vim.api.nvim_create_autocmd("InsertEnter", {
	group = vim.api.nvim_create_augroup("dotfiles-lexima-load", {}),
	callback = function()
		MiniDeps.add("oncomouse/lexima.vim")
		load_lexima()
		vim.g.dotfiles_load_lexima = true
	end,
	once = true,
})

-- }}}
-- nvim-ref {{{
MiniDeps.add("oncomouse/nvim-ref")
MiniDeps.later(function()
	require("nvim-ref").setup({
		bibfiles = {
			vim.fs.joinpath(vim.g.seadrive_path, "My Library/Documents/Academic Stuff/library.bib"),
		},
	})
end)
-- }}}
-- indent-blankline.nvim {{{
MiniDeps.add("lukas-reineke/indent-blankline.nvim")
MiniDeps.later(function()
	require("ibl").setup({
		indent = { char = "▏" },
		scope = { enabled = false },
		exclude = {
			filetypes = {
				"help",
				"alpha",
				"dashboard",
				"neo-tree",
				"Trouble",
				"lazy",
				"mason",
				"notify",
				"toggleterm",
				"lazyterm",
			},
		},
	})
end)
-- }}}
-- zen-mode.nvim {{{
MiniDeps.add("folke/zen-mode.nvim")
MiniDeps.later(function()
	require("zen-mode").setup({
		window = {
			backdrop = 1,
		},
		plugins = {
			tmux = {
				enabled = true,
			},
		},
	})
	vim.keymap.set("n", "<leader>gz", "<cmd>ZenMode<cr>", { desc = "ZenMode" })
end)
-- }}}
-- none-ls.nvim {{{
MiniDeps.add({
	source = "nvimtools/none-ls.nvim",
	depends = {
		"nvimtools/none-ls-extras.nvim",
	},
})
MiniDeps.later(function()
	local sources = {
		-- GENERAL PURPOSE
		require("null-ls").builtins.formatting.prettier.with({
			update_on_insert = false,
			filetypes = {
				"css",
				"graphql",
				"handlebars",
				"html",
				"json",
				"jsonc",
				"less",
				"markdown",
				"markdown.mdx",
				"scss",
				"svelte",
				"typescript",
				"typescript.react",
				"vue",
			},
			extra_args = { "--use-tabs" },
			prefer_local = "node_modules/.bin",
		}),

		-- YAML
		require("null-ls").builtins.formatting.prettier.with({
			filetypes = {
				"yaml",
			},
			prefer_local = "node_modules/.bin",
		}),

		-- LUA
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.diagnostics.selene.with({
			cwd = function(_)
				return vim.fs.dirname(
					vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
				) or vim.fn.expand("~/.config/selene/") -- fallback value
			end,
		}),

		-- PYTHON
		require("none-ls.formatting.ruff"),
		require("none-ls.diagnostics.ruff"),

		-- FISH
		require("null-ls").builtins.formatting.fish_indent,

		-- SHELL
		require("null-ls").builtins.formatting.shfmt,

		-- VIML
		require("null-ls").builtins.diagnostics.vint,

		-- HTML
		require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

		-- JAVASCRIPT DEFAULTS
		require("null-ls").builtins.diagnostics.standardjs,
		require("null-ls").builtins.formatting.standardjs,
	}

	require("null-ls").setup({
		sources = sources,
	})
end)
-- }}}
-- }}}
-- Theme {{{
if not pcall(vim.cmd.colorscheme, "catppuccin") then
	vim.cmd([[colorscheme default]])
end
-- }}}
-- LSP {{{
-- Set to true for debug logging in LSP:
vim.g.dotfiles_lsp_debug = false

-- Use LspAttach event:
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("dotfiles-lsp-on-attach", {}),
	callback = function(ev)
		require("dotfiles.lsp").on_attach(vim.lsp.get_client_by_id(ev.data.client_id), ev.buf)
	end,
})

-- Configure servers here:
vim.g.dotfiles_lsp = {
	cssls = {
		snippets = true,
	},
	eslint = {
		snippets = true,
	},
	html = {
		snippets = true,
	},
	jsonls = {
		filetypes = { "json", "jsonc" },
		snippets = true,
	},
	lua_ls = {
		snippets = true,
		settings = {
			Lua = {
				completion = { callSnippet = "Both" },
				workspace = {
					-- Make the server aware of Neovim runtime files
					checkThirdParty = false,
					library = {
						vim.env.VIMRUNTIME,
					},
				},
				-- Do not send telemetry data containing a randomized but unique identifier
				telemetry = { enable = false },
				runtime = {
					version = "LuaJIT",
				},
			},
		},
	},
	standardrb = {
		single_file_support = true,
	},
	vimls = {
		init_options = {
			isNeovim = true,
			diagnostic = {
				enable = false,
			},
		},
		snippets = true,
	},
}
-- To boot a server, run: require("dotfiles.lsp").start_server(<lspconfig configuration name>) in the appropriate ftplugins file

-- Turn on debug-level logging for LSP:
if vim.g.dotfiles_lsp_debug then
	vim.lsp.set_log_level("trace")
end
-- }}}
-- Diagnostics {{{

-- Configuration
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})

--Signs
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- Add diagnostics to loclist:
vim.api.nvim_create_autocmd({ "DiagnosticChanged" }, {
	group = vim.api.nvim_create_augroup("dotfiles-attach_diagnostics", {}),
	pattern = "*",
	callback = function()
		vim.diagnostic.setloclist({ open = false })
	end,
})

-- }}}
-- Filetypes {{{
vim.filetype.add({
	extension = {
		rasi = "rasi",
	},
	filename = {},
	pattern = {},
})
-- }}}
-- Neovim API Overrides {{{
function vim.print(...)
	if vim.in_fast_event() then
		print(...)
		return ...
	end
	local output = {}
	for i = 1, select("#", ...) do
		local o = select(i, ...)
		if type(o) == "string" then
			table.insert(output, o)
		else
			table.insert(output, vim.inspect(o, { newline = " ", indent = "" }))
		end
	end
	vim.api.nvim_out_write(table.concat(output, "    "))
	vim.api.nvim_out_write("\n")
	return ...
end

-- }}}
-- # vim:foldmethod=marker:foldlevel=0
