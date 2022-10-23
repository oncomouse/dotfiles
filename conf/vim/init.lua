-- Dotfiles Settings {{{
-- Add Dotfiles To RTP:
vim.opt.runtimepath:append("~/dotfiles/conf/vim")
vim.opt.runtimepath:append("~/dotfiles/conf/vim/after")

-- Set Spellfile Location:
vim.opt.spellfile = "~/dotfiles/conf/vim/spell/en.utf-8.add"
-- }}}
-- Basic Settings {{{
vim.cmd([[set visualbell t_vb=]]) -- Disable visual bell
vim.opt.autowrite = true --  Autosave files
vim.opt.hidden = true --  turn off buffer saving when switching
vim.opt.lazyredraw = true --  Don't redraw between macro runs (may make terminal flicker)

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

-- Completion:
vim.opt.completeopt = "menuone,noselect,noinsert,preview"
vim.opt.shortmess = vim.opt.shortmess + "c"
-- prevent a condition where vim lags due to searching include files.
vim.opt.complete = vim.opt.complete - "i"

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

-- Minimal statusline (used if notermguicolors is set):
vim.opt.statusline = " %0.45f%m%h%w%r%= %y %l:%c "

-- Searching:
vim.opt.wrapscan = true -- Start scan over at the top

-- Linewrap:
vim.opt.sidescroll = 5 -- Unused without set wrap, but prepared in case it is used
vim.opt.showbreak = "↳ " -- Show a line has wrapped

-- Mouse And Clipboard:
vim.opt.mouse = "a" -- Mouse support
if vim.fn.has("clipboard") == 1 then
	if vim.fn.has("unnamedplus") == 1 then
		vim.opt.clipboard = "unnamedplus,unnamed"
	else
		vim.opt.clipboard = "unnamed"
	end
end

vim.opt.dictionary = "/usr/share/dict/words"

-- Default to hashtag-style comments, by default:
vim.opt.commentstring = "# %s"

-- Disable Plugins {{{
vim.g.load_black = "py1.0"
vim.g.loaded_fzf = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_remote_plugins = 1
vim.g.loaded_matchit = 1
-- }}}
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
-- }}}
-- Maps {{{

-- Navigation in insert mode:
vim.keymap.set("i", "<C-a>", "<C-o>^", { silent = true })
vim.keymap.set("i", "<C-e>", "<C-o>$", { silent = true })
local function move_char(backwards)
	return function()
		local row, col = unpack(vim.api.nvim_win_get_cursor(0))
		if (backwards and col == 0) or (not backwards and col == #vim.api.nvim_get_current_line()) then
			return
		end
		return vim.api.nvim_win_set_cursor(0, { row, backwards and col - 1 or col + 1 })
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

-- Move lines:
vim.keymap.set("n", "<A-j>", ":move .+1<CR>==", { silent = true, noremap = true })
vim.keymap.set("n", "<A-k>", ":move .-2<CR>==", { silent = true, noremap = true })
vim.keymap.set("v", "<A-j>", ":move '>+1<CR>gv=gv", { silent = true, noremap = true })
vim.keymap.set("v", "<A-k>", ":move '<-2<CR>gv=gv", { silent = true, noremap = true })

-- Clear Currently Highlighted Regexp:
vim.keymap.set("n", "<leader>cr", ':let<C-u>let @/=""<CR>', { silent = true, noremap = true })

-- Jump to last buffer:
vim.keymap.set("n", "``", "<cmd>e #<CR>", { silent = true, noremap = true })

-- Navigate Location List:
vim.keymap.set("n", "]d", "<cmd>lnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[d", "<cmd>lprev<CR>", { silent = true, noremap = true })

-- Toggle Quickfix:
vim.keymap.set("n", "<leader>q", function()
	list_toggle("c")
end, { silent = true, noremap = true })
vim.keymap.set("n", "<leader>d", function()
	list_toggle("l")
end, { silent = true, noremap = true })

-- Project Grep:
vim.keymap.set("n", "<leader>/", function()
	grep_or_qfgrep()
end, { silent = true, noremap = true })

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", function()
	vim.cmd([[echo '@'.getcmdline()
	execute ":'<,'>normal @".nr2char(getchar())]])
end, { silent = true, noremap = true })

-- Calculator:
vim.keymap.set("i", "<C-X><C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- Sourced from jessarcher/dotfiles {{{
--  \ https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim
-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
vim.keymap.set("n", "j", "(v:count == 0 ? 'gj' : 'j')", { silent = true, noremap = true, expr = true })
vim.keymap.set("n", "k", "(v:count == 0 ? 'gk' : 'k')", { silent = true, noremap = true, expr = true })
-- }}}
-- Sourced from romainl/minivimrc {{{
--  \ https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.keymap.set("n", "<localleader>f", ":find *", { noremap = true })
vim.keymap.set("n", "<localleader>s", ":sfind *", { noremap = true })
vim.keymap.set("n", "<localleader>v", ":vert sfind *", { noremap = true })
-- Minimal Buffer Jumping:
vim.keymap.set("n", "<leader>a", ":buffers<CR>:buffer<Space> ", { noremap = true })
vim.keymap.set("n", "<localleader>a", ":buffer *", { noremap = true })
vim.keymap.set("n", "<localleader>A", ":sbuffer *", { noremap = true })
-- }}}

-- }}}
-- Autogroups {{{
vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- }}}
-- Autocommands {{{
-- Line Number Colors in default:
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNr ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrAbove ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrBelow ctermfg=7" }
)
vim.api.nvim_create_autocmd("ColorScheme", {
	group = "dotfiles-settings",
	pattern = "default",
	command = "hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE gui=NONE",
})
vim.api.nvim_create_autocmd("ColorScheme", {
	group = "dotfiles-settings",
	pattern = "default",
	command = "hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE gui=NONE",
})

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
	callback = function()
		vim.highlight.on_yank({ higroup = "IncSearch", timeout = 500 })
	end,
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

-- }}}
-- Theme {{{
-- Fancy color for macs and X11 sessions:
if require("dotfiles.utils.use_termguicolors")() then
	vim.cmd([[let &t_8f='<Esc>[38;2;%lu;%lu;%lum']])
	vim.cmd([[let &t_8b='<Esc>[48;2;%lu;%lu;%lum']])
	vim.opt.termguicolors = true

	local ok = pcall(vim.cmd, [[colorscheme lushwal]])
	if not ok then
		vim.cmd([[colorscheme default]])
	end
else
	vim.cmd([[colorscheme default]])
end
-- }}}
-- Commands {{{

-- Formatting and Diagnostic commands for LSP-less files
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

-- :Git command
vim.api.nvim_create_user_command("Git", function(args)
	if args.args:match("^status") then
		vim.cmd("GitStatus")
	else
		vim.cmd("Gina " .. args.args)
	end
end, {
	force = true,
	nargs = "+",
})

-- }}}
-- Signs {{{
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end
-- }}}
-- Writing {{{
vim.g.bibfiles = "~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib"
-- }}}
-- Plugins {{{
require("dotfiles.plugins")
require("nvim-ref").setup({
	bibfiles = vim.g.bibfiles,
})
-- }}}
-- LSP: {{{
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})

-- LSP Logging:
-- vim.lsp.set_log_level("trace")

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
-- # vim:foldmethod=marker:foldlevel=0
