-- luacheck: globals vim dotfiles
-- Dotfiles Settings: {{{
if not vim.tbl_contains(vim.opt.runtimepath:get(), vim.fn.expand("~/dotfiles/conf/vim")) then
	vim.opt.runtimepath:append("~/dotfiles/conf/vim")
end
local map = require("dotfiles.utils.map")
local xdg = require("dotfiles.utils.xdg")
dotfiles = _G.dotfiles or {}

-- Don't use the minimal minpac install in vimrc-minimal
vim.g.skip_minimal_minpac = 1

-- Load Basic Settings:
vim.cmd([[runtime vimrc-minimal]])

-- Add Dotfiles After To RTP:
vim.opt.runtimepath:append("~/dotfiles/conf/vim/after")

-- Set Spellfile Location:
vim.opt.spellfile = "~/dotfiles/conf/vim/spell/en.utf-8.add"

-- }}}
-- Mac NeoVim Settings: {{{
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
-- Autogroups {{{
vim.cmd([[augroup dotfiles-settings
	autocmd!
augroup END]])
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
-- Folds {{{
vim.cmd([[autocmd dotfiles-settings FileType vim setlocal foldmethod=marker foldlevel=0]])
vim.cmd([[autocmd dotfiles-settings FileType diff setlocal nofoldenable]])
-- }}}
-- Maps {{{
vim.g.enable_todo = 1
-- Highlight a block and type "@" to run a macro on the block:
map.xnoremap("<silent>", "@", ":<C-u>call visualat#execute_macro_over_visual_range()<CR>")

-- Calculator:
map.inoremap("<silent>", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>")

-- Shortcut to view current syntax highlighting group:
vim.cmd[[map <F10> <cmd>echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]]

-- Sourced from jessarcher/dotfiles {{{
--  \ https://github.com/jessarcher/dotfiles/blob/master/nvim/init.vim

-- Allow gf to open non-existent files
map.map("gf", ":edit <cfile><cr>")

-- Quicker switching between windows
map.nmap("<silent>", "<C-h>", "<C-w>h")
map.nmap("<silent>", "<C-j>", "<C-w>j")
map.nmap("<silent>", "<C-k>", "<C-w>k")
map.nmap("<silent>", "<C-l>", "<C-w>l")

-- Reselect visual selection after indenting
map.vnoremap("<", "<gv")
map.vnoremap(">", ">gv")

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
map.noremap("<silent> <expr>", "j", "(v:count == 0 ? 'gj' : 'j')")
map.noremap("<silent> <expr>", "k", "(v:count == 0 ? 'gk' : 'k')")

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
-- Theme: {{{
-- Fancy color for macs and X11 sessions:
if vim.fn.has("mac") == 1 or vim.fn.exists("$DISPLAY") == 1 then
	-- let &t_8f='<Esc>[38;2;%lu;%lu;%lum'
	-- let &t_8b='<Esc>[48;2;%lu;%lu;%lum'
	vim.opt.termguicolors = true

	local wal_cache = vim.fn.expand(xdg("XDG_CACHE_HOME") .. "/wal/vim")
	if vim.fn.isdirectory(wal_cache) == 1 then
		vim.opt.runtimepath:append(wal_cache)
		vim.cmd([[colorscheme wal]])
	else
		vim.cmd([[colorscheme default]])
	end
else
	vim.cmd([[colorscheme default]])
end
-- }}}
-- Other Settings {{{
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

-- Update FASD For NeoVim: {{{
dotfiles.fasd_update = function()
	if vim.fn.empty(vim.opt.buftype:get()) == 1 then
		vim.fn.jobstart({ "fasd", "-A", vim.fn.expand("%:p") })
	end
end
vim.cmd([[autocmd dotfiles-settings BufWinEnter,BufFilePost * call v:lua.dotfiles.fasd_update()]])
-- }}}
-- }}}
-- Writing: {{{
vim.g.bibfiles = "~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib"
-- }}}
-- Plugins {{{
-- Lazy-load packer commands:
vim.cmd([[command! PackerInstall packadd packer.nvim | lua require('dotfiles.plugins').install()]])
vim.cmd([[command! PackerUpdate packadd packer.nvim | lua require('dotfiles.plugins').update()]])
vim.cmd([[command! PackerSync packadd packer.nvim | lua require('dotfiles.plugins').sync()]])
vim.cmd([[command! PackerClean packadd packer.nvim | lua require('dotfiles.plugins').clean()]])
vim.cmd([[command! PackerStatus packadd packer.nvim | lua require('dotfiles.plugins').status()]])
vim.cmd([[command! PackerCompile packadd packer.nvim | lua require('dotfiles.plugins').compile()]])

-- Install packer.nvim, if it isn't present:
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", vim.g.install_path })
	vim.cmd([[PackerSync]])
end
-- }}}
-- # vim:foldmethod=marker:foldlevel=0
