if type(vim.loader) == "table" then
	vim.loader.enable()
end
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
-- if vim.fn.has("nvim-0.9") == 1 then
-- 	vim.opt.showcmdloc = "statusline"
-- end
vim.opt.showmode = false

-- Enable termguicolors by default
vim.opt.termguicolors = true

-- }}}
-- Autogroups: {{{
local augroup = vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- }}}
-- Load lazy.nvim {{{
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
require("lazy").setup({
	-- defaults = {
	-- 	lazy = true,
	-- },
	install = {
		colorscheme = { "catppuccin-mocha" },
	},
	spec = {
		{ import = "dotfiles.plugins" },
	},
	dev = {
		path = vim.fn.expand("~/Projects"),
	},
	performance = {
		rtp = {
			paths = {
				vim.fn.expand("~/dotfiles/conf/vim"),
				vim.fn.expand("~/dotfiles/conf/vim/after"),
			},
			disabled_plugins = {
				"gzip",
				"matchit",
				"matchparen",
				-- "netrwPlugin",
				"tarPlugin",
				"tohtml",
				"tutor",
				"zipPlugin",
			},
		},
	},
})
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
vim.keymap.set("i", "<C-k>", function()
	local _, col = unpack(vim.api.nvim_win_get_cursor(0))
	if col == #vim.api.nvim_get_current_line() then
		return "<c-o>J"
	else
		return "<c-o>D"
	end
end, { expr = true, desc = "Kill rest of line" })

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
-- }}}
-- Plugins {{{
require("rocks") -- Add luarocks to the path
require("select-digraphs").setup({}) -- Configure select-digraphs
-- }}}
-- Theme {{{
if not pcall(vim.cmd.colorscheme, "catppuccin-mocha") then
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
