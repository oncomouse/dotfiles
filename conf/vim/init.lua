vim.g.do_filetype_lua = 1 -- Enable filetype.lua
vim.g.did_load_filetypes = 0 -- Disable filetype.vim
-- Dotfiles Settings {{{
if not vim.tbl_contains(vim.opt.runtimepath:get(), vim.fn.expand("~/dotfiles/conf/vim")) then
	vim.opt.runtimepath:append("~/dotfiles/conf/vim")
end

-- Add Dotfiles After To RTP:
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
vim.opt.wildcharm = vim.fn.char2nr("^Z")
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

-- Linewrap:
-- vim.opt.wrap = false
vim.opt.sidescroll = 5 -- Unused without set wrap, but prepared in case it is used
vim.opt.showbreak = "↳ " -- Show a line has wrapped

-- Listchars:
vim.opt.listchars = "tab:│ ,nbsp:␣,trail:•,precedes:<,extends:>"
vim.opt.list = true

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
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1
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

-- Clear Currently Highlighted Regexp:
vim.keymap.set("n", "<leader>cr", ':let<C-u>let @/=""<CR>', { silent = true, noremap = true })

-- Navigate Buffers:
vim.keymap.set("n", "]b", "<cmd>bnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[b", "<cmd>bprevious<CR>", { silent = true, noremap = true })
--
-- Jump to the alternate buffer:
vim.keymap.set("n", "``", "<cmd>e #<CR>", { silent = true, noremap = true })

-- Source https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.keymap.set("n", "<localleader>f", ":find *", { noremap = true })
vim.keymap.set("n", "<localleader>s", ":sfind *", { noremap = true })
vim.keymap.set("n", "<localleader>v", ":vert sfind *", { noremap = true })
-- Minimal Buffer Jumping:
vim.keymap.set("n", "<leader>a", ":buffers<CR>:buffer<Space> ", { noremap = true })
vim.keymap.set("n", "<localleader>a", ":buffer *", { noremap = true })
vim.keymap.set("n", "<localleader>A", ":sbuffer *", { noremap = true })

-- Navigate Quickfix:
vim.keymap.set("n", "]q", "<cmd>cnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[q", "<cmd>cprevious<CR>", { silent = true, noremap = true })

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

-- Enable Todo:
vim.g.enable_todo = 1

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", require("dotfiles.visualat"), { silent = true, noremap = true })

-- Calculator:
vim.keymap.set("i", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- Sourced from jessarcher/dotfiles {{{
--  \ https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim

-- Reselect visual selection after indenting
-- vim.keymap.set("v", "<", "<gv", { noremap = true })
-- vim.keymap.set("v", ">", ">gv", { noremap = true })

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
vim.keymap.set("n", "j", "(v:count == 0 ? 'gj' : 'j')", { silent = true, noremap = true, expr = true })
vim.keymap.set("n", "k", "(v:count == 0 ? 'gk' : 'k')", { silent = true, noremap = true, expr = true })

-- }}}
-- FZF Bindings: {{{
vim.keymap.set("n", "<c-p>", "<cmd>Files<CR>", { silent = true })
vim.keymap.set("n", "<leader>a", "<cmd>Buffers<CR>", { silent = true })
-- }}}
-- Textobjects: {{{
-- Fold Maps:
vim.keymap.set("o", "iz", "<cmd>normal! [zj0v]zk$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "iz", "<cmd>normal! [zj0o]zk$<cr>", { silent = true, noremap = true })
vim.keymap.set("o", "az", "<cmd>normal! [zv]z$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "az", "<cmd>normal! [zo]z$<cr>", { silent = true, noremap = true })

-- Entire document:
vim.keymap.set("o", "ae", "<cmd>normal! gg0vG$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "ae", "<cmd>normal! gg0oG$<cr>", { silent = true, noremap = true })
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
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{
		group = "dotfiles-settings",
		pattern = "default",
		command = "hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE gui=NONE",
	}
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{
		group = "dotfiles-settings",
		pattern = "default",
		command = "hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE gui=NONE",
	}
)

-- Turn Off Line Numbering:
if vim.fn.has("nvim") == 1 then
	vim.api.nvim_create_autocmd(
		"TermOpen",
		{ group = "dotfiles-settings", command = "setlocal nonumber norelativenumber" }
	)
end

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
vim.api.nvim_create_user_command("Format", "silent normal! mxgggqG`x<CR>", {
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

-- Custom :Git command to utilize fzf-lua for status
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
-- Lazy-load packer commands:
local packer_commands = {
	"install",
	"update",
	"sync",
	"clean",
	"status",
	"compile",
}
for _, cmd in pairs(packer_commands) do
	vim.api.nvim_create_user_command("Packer" .. cmd:gsub("^%l", string.upper), function()
		vim.cmd("packadd packer.nvim")
		require("dotfiles.plugins")[cmd]()
	end, {})
end

-- Update Packer.nvim automatically:
vim.api.nvim_create_autocmd("BufWritePost", {
	group = "dotfiles-settings",
	pattern = "plugins/*.lua",
	command = "source <afile> | PackerCompile",
})
-- Install packer.nvim, if it isn't present:
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.jobstart({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }, {
		on_exit = function()
			vim.cmd("packadd packer.nvim")
			vim.cmd("PackerSync")
		end,
	})
end
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
