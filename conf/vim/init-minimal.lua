--------------------------------------------------------------------------------
-- Plugins for lazy.nvim
--------------------------------------------------------------------------------

local plugins = {
	{ "folke/lazy.nvim" },
	{
		"oncomouse/catppuccin.nvim",
		event = "VeryLazy",
		opts = {
			integrations = {
				mini = true,
			},
		},
	}, -- colors
	{ "tpope/vim-repeat", event = "VeryLazy" }, -- dot repeat for plugins
	{ "tpope/vim-unimpaired", event = "VeryLazy" }, -- paired mappings, ]b,]q,]l, etc
	{ "tpope/vim-sleuth", event = "VeryLazy" }, -- guess indentation
	{ "christoomey/vim-sort-motion", event = "VeryLazy" }, -- gs to sort
	{ "vim-scripts/ReplaceWithRegister", event = "VeryLazy" }, -- gr{motion} or grr or gr in visual to replace with register
	{ "oncomouse/lazygit.nvim", cmd = "LazyGit" }, -- :LazyGit for lazygit integration
	{ "echasnovski/mini.nvim", lazy = true }, -- Various (see below)
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = true,
		build = function()
			vim.cmd([[TSUpdate]])
		end,
	}, -- Syntax
}

--------------------------------------------------------------------------------
-- Minimal settings to get lazy.nvim workin
--------------------------------------------------------------------------------
-- Autogroups:
vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- Set Leader:
vim.g.mapleader = " "
-- FZF Path:
vim.g.fzf_dir = nil
if vim.fn.isdirectory("/usr/local/opt/fzf") == 1 then -- Homebrew
	vim.g.fzf_dir = "/usr/local/opt/fzf"
elseif vim.fn.isdirectory("/usr/share/vim/vimfiles") == 1 then -- Arch, btw
	vim.g.fzf_dir = "/usr/share/vim/vimfiles"
elseif vim.fn.isdirectory("/usr/share/doc/fzf/examples") == 1 then -- Debian
	vim.g.fzf_dir = "/usr/share/doc/fzf/examples"
elseif vim.fn.isdirectory("~/.fzf") == 1 then -- Local install
	vim.g.fzf_dir = "~/.fzf"
end
--------------------------------------------------------------------------------
-- Load lazy.nvim
--------------------------------------------------------------------------------
local function xdg_default(v, d)
	local o = os.getenv(v)
	return o and o or os.getenv("HOME") .. d
end
local xdg = function(var_name)
	if var_name == "XDG_CONFIG_HOME" then
		return xdg_default("XDG_CONFIG_HOME", "/.config")
	elseif var_name == "XDG_CACHE_HOME" then
		return xdg_default("XDG_CACHE_HOME", "/.cache")
	elseif var_name == "XDG_DATA_HOME" then
		return xdg_default("XDG_DATA_HOME", "/.local/share")
	end
	return nil
end

local lazypath = xdg("XDG_DATA_HOME") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup(plugins, {
	root = vim.fn.stdpath("data") .. "lazy-minimal",
	install = {
		colorscheme = { "catppuccin" },
	},
	performance = {
		dev = {
			path = "~/Projects",
		},
		rtp = {
			paths = {
				vim.g.fzf_dir,
			},
			disabled_plugins = {
				"gzip",
				-- "matchit",
				-- "matchparen",
				"netrwPlugin",
				"tarPlugin",
				"tohtml",
				"tutor",
				"zipPlugin",
			},
		},
	},
})

--------------------------------------------------------------------------------
-- Settings:
--------------------------------------------------------------------------------

vim.cmd([[set visualbell t_vb=]]) -- Disable visual bell
vim.opt.autowrite = true --	 Autosave files
vim.opt.hidden = true --	turn off buffer saving when switching
vim.opt.lazyredraw = true --	Don't redraw between macro runs (may make terminal flicker)

-- Override Default Split Creation Locations:
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Line Numbering:
vim.opt.number = true
vim.opt.relativenumber = true

-- Folds:
vim.opt.foldlevel = 99
vim.opt.foldmethod = "manual"

-- Avoid Highlighting Large Files:
vim.g.large_file = 20 * 1024 * 1024

-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Use split for search/replace preview:
vim.opt.inccommand = "split"

-- Height Of The Preview Window:
vim.opt.previewheight = 14

-- Listchars:
vim.opt.list = true

-- Completion:
vim.opt.completeopt = "menuone,noselect,noinsert,preview"
vim.opt.shortmess:append("Wc")
-- prevent a condition where vim lags due to searching include files.
vim.opt.complete:remove("i")

-- <C-z> expands wildcards in command mode
vim.cmd([[set wildcharm=<C-z>]])
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

-- Searching:
vim.opt.wrapscan = true -- Start scan over at the top

vim.opt.dictionary = "/usr/share/dict/words"

-- Default to hashtag-style comments, by default:
vim.opt.commentstring = "# %s"

-- Minimal Statusbar:
vim.opt.statusline = " %0.45f%m%h%w%r%= %y %l:%c "

-- Mouse And Clipboard:
vim.opt.mouse = "a" -- Mouse support
if vim.fn.has("clipboard") == 1 then
	if vim.fn.has("unnamedplus") == 1 then
		vim.opt.clipboard = "unnamedplus,unnamed"
	else
		vim.opt.clipboard = "unnamed"
	end
end

-- Options taken from mini.basics, which does too many things I don't want:
vim.opt.virtualedit = "block"
vim.opt.formatoptions = "qjl1"

-- Neovim 0.9 Dependent Settings:
if vim.fn.has("nvim-0.9") == 1 then
	-- Add some other Neovim 0.9 things here:
	vim.opt.shortmess = vim.opt.shortmess + "C"
	vim.opt.splitkeep = "screen"
end

--------------------------------------------------------------------------------
-- Tabs:
--------------------------------------------------------------------------------

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false

--------------------------------------------------------------------------------
-- Functions:
--------------------------------------------------------------------------------

local function set_list_chars()
	if vim.opt.expandtab:get() then
		vim.opt_local.listchars = vim.opt_local.listchars
			+ ("multispace:>" .. vim.fn["repeat"](" ", vim.opt.shiftwidth:get() - 1))
	end
end

-- Open or close quickfix or loclist
local function list_toggle(pfx, force_open)
	if not force_open then
		local status = vim.g["dotfiles_" .. pfx .. "open"] or 0
		if status ~= 0 then
			vim.g["dotfiles_" .. pfx .. "open"] = 0
			vim.cmd(pfx .. "close")
			return
		end
		if pfx == "l" and vim.fn.len(vim.fn.getloclist(0)) == 0 then
			vim.cmd([[echohl ErrorMsg
			echo 'Location List is Empty.'
			echohl NONE]])
			return
		end
	end
	vim.g["dotfiles_" .. pfx .. "open"] = 1
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

--------------------------------------------------------------------------------
-- Autocommands:
--------------------------------------------------------------------------------

vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })

-- Turn Off Line Numbering:
vim.api.nvim_create_autocmd("TermOpen", {
	group = "dotfiles-settings",
	command = "setlocal nonumber norelativenumber",
})

-- Start QuickFix:
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "[^l]*",
	callback = function()
		list_toggle("c", 1)
	end,
})
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "l*",
	callback = function()
		list_toggle("l", 1)
	end,
})

-- Highlighted Yank:
-- vim.api.nvim_create_autocmd("TextYankPost", {
-- 	group = "dotfiles-settings",
-- 	command = [[silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}]],
-- })

-- Close Preview Window:
vim.api.nvim_create_autocmd("CompleteDone", {
	group = "dotfiles-settings",
	callback = function()
		if vim.fn.pumvisible() == 0 then
			vim.cmd("pclose")
		end
	end,
})

-- This Fixes A Mistake In Neovim:
vim.api.nvim_create_autocmd("ColorScheme", {
	group = "dotfiles-settings",
	command = "hi link Whitespace SpecialKey",
	pattern = "*",
})

vim.api.nvim_create_autocmd("BufEnter", {
	group = "dotfiles-settings",
	callback = set_list_chars,
	pattern = "*",
})
--------------------------------------------------------------------------------
-- Maps:
--------------------------------------------------------------------------------
-- Navigation in insert mode:
vim.keymap.set("i", "<C-a>", "<C-o>^", { silent = true })
vim.keymap.set("i", "<C-e>", "<C-o>$", { silent = true })
local function move_char(backwards)
	return function()
		local _, col = unpack(vim.api.nvim_win_get_cursor(0))
		if (backwards and col == 0) or (not backwards and col == #vim.api.nvim_get_current_line()) then
			return ""
		end
		if backwards and col == 1 then
			return "<C-o>^"
		end
		if backwards and col == #vim.api.nvim_get_current_line() then
			return "<C-o>i"
		end
		if not backwards and col == #vim.api.nvim_get_current_line() - 1 then
			return "<C-o>$"
		end
		return "<C-o>" .. (backwards and "h" or "l")
	end
end
vim.keymap.set("i", "<C-b>", move_char(true))
vim.keymap.set("i", "<C-f>", move_char())
local function move_word(backwards)
	return function()
		local _, new_position = unpack(
			vim.fn.searchpos(backwards and [[\<]] or [[\>]], backwards and "bn" or "n", vim.fn.line("."))
		)
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if new_position == 0 then
			col = backwards and 0 or #vim.api.nvim_get_current_line()
		else
			col = new_position - 1
		end
		vim.api.nvim_win_set_cursor(0, { row, col })
	end
end
vim.keymap.set("i", "<A-b>", move_word(true))
vim.keymap.set("i", "<A-f>", move_word())

-- Clear Currently Highlighted Regexp:
vim.keymap.set("n", "<leader>cr", ':let<C-u>let @/=""<CR>', { silent = true, noremap = true })

-- Source https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.keymap.set("n", "<localleader>f", ":find *", { noremap = true })
vim.keymap.set("n", "<localleader>s", ":sfind *", { noremap = true })
vim.keymap.set("n", "<localleader>v", ":vert sfind *", { noremap = true })
-- Minimal Buffer Jumping:
vim.keymap.set("n", "<leader>a", function()
	local last = vim.fn.getbufinfo("#")[1]
	return ":buffers<CR>:buffer<Space>" .. (last ~= nil and last.bufnr or "")
end, {
	noremap = true,
	expr = true,
})
vim.keymap.set("n", "<localleader>a", ":buffer *", { noremap = true })
vim.keymap.set("n", "<localleader>A", ":sbuffer *", { noremap = true })

-- Toggle Quickfix:
vim.keymap.set("n", "<leader>q", function()
	list_toggle("c")
end, { silent = true, noremap = true })
vim.keymap.set("n", "<leader>d", function()
	list_toggle("l")
end, { silent = true, noremap = true })

-- Project Grep:
vim.keymap.set("n", "<leader>/", grep_or_qfgrep, { silent = true, noremap = true })

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", function()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end, {
	silent = true,
	noremap = true,
})

-- Calculator:
vim.keymap.set("i", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- Jump to last buffer:
vim.keymap.set("n", "<leader>b", "<cmd>b#<cr>")
--------------------------------------------------------------------------------
-- Commands:
--------------------------------------------------------------------------------
vim.api.nvim_create_user_command("Diagnostics", function()
	vim.cmd("silent lmake! %")
	if #vim.fn.getloclist(0) == 0 then
		vim.cmd("lopen")
	else
		vim.cmd("lclose")
	end
end, {
	force = true,
})
vim.api.nvim_create_user_command("Format", function()
	vim.api.nvim_feedkeys("mxgggqG`x", "x", true)
end, {
	force = true,
})

-- Adjust Spacing:
vim.api.nvim_create_user_command("Spaces", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = true
	vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│" .. vim.fn["repeat"](" ", args.args)
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
	vim.cmd("silent execute '%!unexpand -t" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})

--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------

local ok = pcall(vim.cmd, [[colorscheme catppuccin-mocha]])
if not ok then
	vim.cmd([[colorscheme default]])
end

--------------------------------------------------------------------------------
-- FZF:
--------------------------------------------------------------------------------

vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = "top" } }
vim.g.fzf_action = {
	["ctrl-s"] = "split",
	["ctrl-v"] = "vsplit",
	["ctrl-t"] = "tabnew",
	["ctrl-e"] = "edit",
}
vim.g.fzf_nvim_statusline = 0
vim.g.fzf_colors = {
	fg = { "fg", "Normal" },
	bg = { "bg", "Normal" },
	hl = { "fg", "Comment" },
	["fg+"] = { "fg", "CursorLine", "CursorColumn", "Normal" },
	["bg+"] = { "bg", "CursorLine", "CursorColumn" },
	["hl+"] = { "fg", "Statement" },
	info = { "fg", "PreProc" },
	border = { "fg", "Ignore" },
	prompt = { "fg", "Conditional" },
	pointer = { "fg", "Exception" },
	marker = { "fg", "Keyword" },
	spinner = { "fg", "Label" },
	header = { "fg", "Comment" },
}
if vim.g.has_fzf ~= 0 then
	vim.keymap.set("n", "<C-P>", "<cmd>FZF --reverse --info=inline<cr>", { silent = true, noremap = true })
end

--------------------------------------------------------------------------------
-- Mini.nvim:
--------------------------------------------------------------------------------

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
	group = "dotfiles-settings",
	pattern = vim.fn.join(vim.tbl_keys(custom_textobjects), ","),
	callback = function()
		local ft = vim.opt.filetype:get()
		vim.b.miniai_config = {
			custom_textobjects = custom_textobjects[ft],
		}
	end,
})

-- ga and gA for alignment:
require("mini.align").setup({})

-- mini.basics:
require("mini.basics").setup({
	options = {
		basic = false,
	},
	mappings = {
		move_with_alt = true,
		windows = true,
	},
})

-- :Bd[!] for layout-safe bufdelete
require("mini.bufremove").setup({})
vim.api.nvim_create_user_command("Bd", function(args)
	require("mini.bufremove").delete(0, not args.bang)
end, {
	bang = true,
})

-- gc for commenting/uncommenting:
require("mini.comment").setup({})

-- We just use this for the indent textobjects:
require("mini.indentscope").setup({})
vim.g.miniindentscope_disable = true

require("mini.move").setup({})

-- May as well setup a minimal autopair:
require("mini.pairs").setup({})

require("mini.statusline").setup({})

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
vim.keymap.del("x", "ys")
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
	group = "dotfiles-settings",
	pattern = vim.fn.join(vim.tbl_keys(custom_surroundings), ","),
	callback = function()
		local ft = vim.opt.filetype:get()
		vim.b.minisurround_config = {
			custom_surroundings = custom_surroundings[ft],
		}
	end,
})

--------------------------------------------------------------------------------
-- Treesitter:
--------------------------------------------------------------------------------

local parsers = {
	"bash",
	"bibtex",
	"c",
	"cmake",
	"comment",
	"cpp",
	"css",
	"dockerfile",
	"fish",
	"go",
	"html",
	"http",
	"java",
	"javascript",
	"json",
	"jsonc",
	"jsdoc",
	"latex",
	"lua",
	"markdown",
	"markdown_inline",
	"make",
	"ninja",
	"perl",
	"php",
	"python",
	"ruby",
	"typescript",
	"vim",
	"xml",
	"yaml",
}
require("nvim-treesitter.parsers").list.xml = {
	install_info = {
		url = "https://github.com/Trivernis/tree-sitter-xml",
		files = { "src/parser.c" },
		generate_requires_npm = true,
		branch = "main",
	},
	filetype = "xml",
}
require("nvim-treesitter.configs").setup({
	ensure_installed = parsers,
	highlight = { enable = true },
})
