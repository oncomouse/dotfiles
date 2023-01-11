local plugins = {
	{ "savq/paq-nvim", opt = true, as = "paq" },
	"lodestone/lodestone.vim", -- colors
	"tpope/vim-repeat", -- dot repeat for plugins
	"tpope/vim-unimpaired", -- paired mappings, ]b,]q,]l, etc
	"tpope/vim-sleuth", -- guess indentation
	"christoomey/vim-sort-motion", -- gs to sort
	"vim-scripts/ReplaceWithRegister", -- gr{motion} or grr or gr in visual to replace with register
	"echasnovski/mini.nvim", -- Various (see below)
	{
		"nvim-treesitter/nvim-treesitter",
		run = function()
			if pcall(require, "nvim-treesitter.require") then
				vim.cmd([[TSUpdate]])
			end
		end,
	}, -- Syntax
}

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
vim.opt.shortmess:append("c")
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

--------------------------------------------------------------------------------
-- Tabs:
--------------------------------------------------------------------------------

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false

--------------------------------------------------------------------------------
-- Disable Plugins:
--------------------------------------------------------------------------------

vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_remote_plugins = 1

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
vim.api.nvim_create_autocmd("TermOpen", { group = "dotfiles-settings", command = "setlocal nonumber norelativenumber" })

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
vim.api.nvim_create_autocmd("TextYankPost", {
	group = "dotfiles-settings",
	command = [[silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}]],
})

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
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if (backwards and col == 0) or (not backwards and col == #vim.api.nvim_get_current_line()) then
			return
		end
		return vim.api.nvim_win_set_cursor(0, {row, backwards and col - 1 or col + 1})
	end
end
vim.keymap.set("i", "<C-b>", move_char(true))
vim.keymap.set("i", "<C-f>", move_char())
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
end, { noremap = true, expr = true })
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
end, { silent = true, noremap = true })

-- Calculator:
vim.keymap.set("i", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
vim.keymap.set("n", "j", "(v:count == 0 ? 'gj' : 'j')", { silent = true, noremap = true, expr = true })
vim.keymap.set("n", "k", "(v:count == 0 ? 'gk' : 'k')", { silent = true, noremap = true, expr = true })

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
-- paq-nvim For Essentials:
--------------------------------------------------------------------------------

function paq_path()
	local o = os.getenv("XDG_DATA_HOME")
	o = o and o or os.getenv("HOME") .. "/.local/share"
	return o .. "/paq.nvim/minimal"
end

function paq_init()
	local paq_dir = paq_path()
	vim.cmd([[packadd paq]])
	local paq = require("paq")
	paq:setup({
		path = paq_dir .. "/pack/paqs/",
	})
	paq(plugins)
	return paq
end

vim.opt.packpath:append(paq_path())
local commands = {
	"sync",
	"clean",
	"install",
	"update",
	"list",
}
for _, command in pairs(commands) do
	vim.api.nvim_create_user_command("Paq" .. command:sub(1, 1):upper() .. command:sub(2), function()
		local paq = paq_init()
		paq[command]()
	end, {
		force = true,
	})
end

local paq_dir = paq_path()
local clone_path = paq_dir .. "/pack/paqs/opt/paq"
local paq_available = true
if vim.fn.empty(vim.fn.glob(clone_path)) == 1 then
	os.execute("git clone --depth 1 https://github.com/savq/paq-nvim " .. clone_path)
	paq_available = false
	local paq = paq_init()
	paq["install"]()
end

-- Try to reload configuration file when paq-nvim is done installing:
vim.api.nvim_create_autocmd("User", {
	pattern = "PaqDoneInstall",
	callback = function()
		if not paq_available then
			vim.cmd([[source $MYVIMRC]])
		end
	end
})
--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------

local ok = pcall(vim.cmd, [[colorscheme lodestone]])
if not ok then
	vim.cmd([[colorscheme default]])
end

--------------------------------------------------------------------------------
-- FZF:
--------------------------------------------------------------------------------

if vim.fn.isdirectory("/usr/local/opt/fzf") == 1 then -- Homebrew
	vim.opt.runtimepath:append("/usr/local/opt/fzf")
	vim.g.has_fzf = 1
elseif vim.fn.isdirectory("/usr/share/vim/vimfiles") == 1 then -- Arch, btw
	vim.opt.runtimepath:append("/usr/share/vim/vimfiles")
	vim.g.has_fzf = 1
elseif vim.fn.isdirectory("/usr/share/doc/fzf/examples") == 1 then -- Debian
	vim.opt.runtimepath:append("/usr/share/doc/fzf/examples")
	vim.g.has_fzf = 1
elseif vim.fn.isdirectory("~/.fzf") == 1 then -- Local install
	vim.opt.runtimepath:append("~/.fzf")
	vim.g.has_fzf = 1
end
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

if paq_available then
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

	-- :Bd[!] for layout-safe bufdelete
	require("mini.bufremove").setup({})
	vim.api.nvim_create_user_command("Bd", function(args)
		require("mini.bufremove").delete(0, not args.bang)
	end, { bang = true })

	-- gc for commenting/uncommenting:
	require("mini.comment").setup({})

	-- We just use this for the indent textobjects:
	require("mini.indentscope").setup({})
	vim.g.miniindentscope_disable = true
	
	-- May as well setup a minimal autopair:
	require("mini.pairs").setup({})

	-- Replace vim-surround:
	require("mini.surround").setup({
		custom_surroundings = {
			["q"] = {
				input = { "“().-()”" },
				output = { left = "“", right = "”" },
			},
		},
		n_lines = 50,
		search_method = "cover_or_next",
	})
	-- Remap adding surrounding to Visual mode selection
	vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })

	-- Per-file surroundings:
	local custom_surroundings = {
		lua = {
			s = {
				input = { "%[%[().-()%]%]" },
				output = { left = "[[", right = "]]" },
			},
		},
		markdown = {
			["b"] = { -- Surround for bold
				input = { "%*%*().-()%*%*" },
				output = { left = "**", right = "**" },
			},
			["i"] = { -- Surround for italics
				input = { "%*().-()%*" },
				output = { left = "*", right = "*" },
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
end

--------------------------------------------------------------------------------
-- Treesitter:
--------------------------------------------------------------------------------

if paq_available then
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
end
