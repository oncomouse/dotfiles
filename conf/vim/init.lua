-- luacheck: globals vim dotfiles
-- Dotfiles Settings {{{
if not vim.tbl_contains(vim.opt.runtimepath:get(), vim.fn.expand("~/dotfiles/conf/vim")) then
	vim.opt.runtimepath:append("~/dotfiles/conf/vim")
end
local map = require("dotfiles.utils.map")
dotfiles = _G.dotfiles or {}

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

if vim.fn.executable("rg") == 1 then
	vim.opt.grepprg = "rg --vimgrep"
	vim.opt.grepformat = "%f:%l:%c:%m"
elseif vim.fn.executable("ag") == 1 then
	vim.opt.grepprg = "ag --vimgrep"
	vim.opt.grepformat = "%f:%l:%c:%m"
else
	vim.opt.grepprg = "grep -rn"
end

vim.opt.wrapscan = true

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
vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_remote_plugins = 1
vim.g.loaded_matchparen = 1
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
-- Statusline {{{
require("dotfiles.statusline")
-- }}}
-- Tabs {{{
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false
-- }}}
-- Maps {{{

-- Select the Whole File:
map.nnoremap("<leader>vf", "ggVG")

-- Clear Currently Highlighted Regexp:
map.nnoremap("<silent>", "<leader>cr", ':let<C-u>let @/=""<CR>')

-- Navigate Buffers:
map.nnoremap("<silent>", "]b", "<cmd>bnext<CR>")
map.nnoremap("<silent>", "[b", "<cmd>bprevious<CR>")
--
-- Jump to the alternate buffer:
map.nnoremap("<silent>", "``", "<cmd>e #<CR>")

-- Source https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
map.nnoremap("<localleader>f", ":find *")
map.nnoremap("<localleader>s", ":sfind *")
map.nnoremap("<localleader>v", ":vert sfind *")
-- Minimal Buffer Jumping:
map.nnoremap("<leader>a", ":buffers<CR>:buffer<Space> ")
map.nnoremap("<localleader>a", ":buffer *")
map.nnoremap("<localleader>A", ":sbuffer *")

-- Better Matching:
map.nnoremap("[I", "[I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>")
map.nnoremap("]I", "]I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>")

-- Navigate Quickfix:
map.nnoremap("<silent>", "]q", "<cmd>cnext<CR>")
map.nnoremap("<silent>", "[q", "<cmd>cprevious<CR>")

-- Navigate Location List:
map.nnoremap("<silent>", "]d", "<cmd>lnext<CR>")
map.nnoremap("<silent>", "[d", "<cmd>lprev<CR>")

-- Toggle Quickfix:
map.nnoremap("<silent>", "<leader>q", "<cmd>lua dotfiles.list_toggle('c')<CR>")
map.nnoremap("<silent>", "<leader>d", "<cmd>lua dotfiles.list_toggle('l')<CR>")

-- Project Grep:
map.nnoremap("<silent>", "<leader>/", "<cmd>lua dotfiles.grep_or_qfgrep()<CR>")

-- Enable Todo:
vim.g.enable_todo = 1

-- Highlight a block and type "@" to run a macro on the block:
map.xnoremap("<silent>", "@", ":<C-u>call visualat#execute_macro_over_visual_range()<CR>")

-- Calculator:
map.inoremap("<silent>", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>")

-- Shortcut to view current syntax highlighting group:
vim.cmd([[map <F10> <cmd>echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]])

-- Sourced from jessarcher/dotfiles {{{
--  \ https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim

-- Quicker switching between windows
map.nmap("<silent>", "<C-h>", "<C-w>h")
map.nmap("<silent>", "<C-j>", "<C-w>j")
map.nmap("<silent>", "<C-k>", "<C-w>k")
map.nmap("<silent>", "<C-l>", "<C-w>l")

-- Reselect visual selection after indenting
map.vnoremap("<", "<gv")
map.vnoremap(">", ">gv")

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
map.nnoremap("<silent> <expr>", "j", "(v:count == 0 ? 'gj' : 'j')")
map.nnoremap("<silent> <expr>", "k", "(v:count == 0 ? 'gk' : 'k')")

-- Paste replace visual selection without copying it
map.vnoremap("<leader>p", '"_dP')
-- }}}
-- FZF Bindings: {{{
map.nmap("<silent>", "<c-p>", "<cmd>Files<CR>")
map.nmap("<silent>", "<leader>a", "<cmd>Buffers<CR>")
-- }}}
-- Fold Maps: {{{
map.onoremap("<silent>", "iz", "<cmd>normal! [zj0v]zk$<cr>")
map.xnoremap("<silent>", "iz", "<cmd>normal! [zj0o]zk$<cr>")
map.onoremap("<silent>", "az", "<cmd>normal! [zv]z$<cr>")
map.xnoremap("<silent>", "az", "<cmd>normal! [zo]z$<cr>")
-- }}}
-- }}}
-- Theme {{{
-- Fancy color for macs and X11 sessions:
if vim.fn.has("mac") == 1 or vim.fn.exists("$DISPLAY") == 1 then
	vim.cmd([[let &t_8f='<Esc>[38;2;%lu;%lu;%lum']])
	vim.cmd([[let &t_8b='<Esc>[48;2;%lu;%lu;%lum']])
	vim.opt.termguicolors = true

	vim.cmd([[colorscheme lushwal]])
else
	vim.cmd([[colorscheme default]])
end
-- }}}
-- Autogroups {{{
vim.cmd([[augroup dotfiles-settings
	autocmd!
augroup END]])
-- }}}
-- Autocommands {{{
-- Line Number Colors in default:
vim.cmd([[autocmd dotfiles-settings ColorScheme default hi LineNr ctermfg=7]])
vim.cmd([[autocmd dotfiles-settings ColorScheme default hi LineNrAbove ctermfg=7]])
vim.cmd([[autocmd dotfiles-settings ColorScheme default hi LineNrBelow ctermfg=7]])
vim.cmd([[autocmd dotfiles-settings ColorScheme default hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE]])
vim.cmd([[autocmd dotfiles-settings ColorScheme default hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE]])

-- Turn Off Line Numbering:
if vim.fn.has("nvim") == 1 then
	vim.cmd([[autocmd dotfiles-settings TermOpen * setlocal nonumber norelativenumber]])
end

-- Start QuickFix:
vim.cmd([[autocmd dotfiles-settings QuickFixCmdPost [^l]* lua dotfiles.list_toggle('c', 1)]])
vim.cmd([[autocmd dotfiles-settings QuickFixCmdPost l*    lua dotfiles.list_toggle('l', 1)]])

-- Highlighted Yank:
vim.cmd(
	[[autocmd dotfiles-settings TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}]]
)

-- Close Preview Window:
vim.cmd([[autocmd dotfiles-settings CompleteDone * if pumvisible() == 0 | pclose | endif]])

-- On opening a file, jump to the last known cursor position (see :h line())
vim.cmd([[autocmd dotfiles-settings BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit' |
\	 exe "normal! g`\"" |
\ end]])

-- Fix window resizing
-- vim.cmd([[autocmd dotfiles-settings VimEnter * silent exec "!kill -s SIGWINCH $PPID"]])

-- Update FASD For NeoVim
vim.cmd([[autocmd dotfiles-settings BufWinEnter,BufFilePost * lua dotfiles.fasd_update()]])
-- }}}
-- Commands {{{
vim.api.nvim_add_user_command("Diagnostics", function()
	vim.cmd("silent lmake! %")
	if #vim.fn.getloclist(0) == 0 then
		vim.cmd("lopen")
	else
		vim.cmd("lclose")
	end
end, {})
vim.api.nvim_add_user_command("Format", "silent normal! mxgggqG`x<CR>", {})

-- Adjust Spacing:
vim.api.nvim_add_user_command("Spaces", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = true
	vim.cmd("silent execute '%!expand -it" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	nargs = 1,
})
vim.api.nvim_add_user_command("Tabs", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = false
	vim.cmd("silent execute '%!unexpand -t" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	nargs = 1,
})
-- }}}
-- Functions {{{
-- Hide or display a quickfix or location list:
dotfiles.list_toggle = function(pfx, force_open)
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

-- Search project directory or, if we are in a quickfix buffer, search there:
dotfiles.grep_or_qfgrep = function()
	if vim.opt.buftype == "quickfix" then
		-- Load cfilter in quickfix view:
		vim.cmd([[packadd cfilter]])
		local input = vim.fn.input("QFGrep/")
		if #input > 0 then
			local prefix = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist and "L" or "C"
			vim.cmd(prefix .. "filter /" .. input .. "/")
		end
	else
		local input = vim.fn.input("Grep/")
		if #input > 0 then
			vim.cmd('silent! grep! "' .. input .. '"')
		end
	end
end

-- Update FASD for Neovim:
dotfiles.fasd_update = function()
	if vim.fn.empty(vim.opt.buftype:get()) == 1 then
		vim.fn.jobstart({ "fasd", "-A", vim.fn.expand("%:p") })
	end
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
	vim.api.nvim_add_user_command("Packer" .. cmd:gsub("^%l", string.upper), function()
		vim.cmd("packadd packer.nvim")
		require("dotfiles.plugins")[cmd]()
	end, {})
end

-- Update Packer.nvim automatically:
vim.cmd([[autocmd! dotfiles-settings BufWritePost plugins.lua source <afile> | PackerCompile]])

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
-- # vim:foldmethod=marker:foldlevel=0
