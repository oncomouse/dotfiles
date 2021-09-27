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
vim.cmd[[map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>]]

-- Uniform Visual Motion Toggle: {{{
map.map("<leader>w", "<cmd>call edit_mode#toggle()<CR>")
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
-- Lexima Configuration {{{
local function make_rule(at, ed, filetype, syntax)
	return {
		char = "<CR>",
		input = "<CR>",
		input_after = "<CR>" .. ed,
		at = at,
		except = "\\C\\v^(\\s*)\\S.*%#\\n%(%(\\s*|\\1\\s.+)\\n)*\\1" .. ed,
		filetype = filetype,
		syntax = syntax,
	}
end
function dotfiles.lexima_extend()
	-- Lua end rules:
	vim.fn["lexima#add_rule"](
		make_rule("^\\s*if\\>.*then\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {})
	)
	vim.fn["lexima#add_rule"](
		make_rule("^\\s*\\%(for\\|while\\)\\>.*do\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {})
	)
	vim.fn["lexima#add_rule"](
		make_rule("^\\s*\\%(local\\)\\=.*function\\>\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {})
	)
end
vim.cmd([[autocmd! dotfiles-settings FileType lua lua dotfiles.lexima_extend()]])
map.inoremap("<silent>", "<Plug>(dotfiles-lexima)", [[<C-r>=lexima#insmode#leave_till_eol("")<CR>]])
map.imap("<silent>", "<C-l>", "<Plug>(dotfiles-lexima)")
-- }}}
-- Plugins {{{
-- Lazy-load packer commands:
vim.cmd([[command! PackerInstall packadd packer.nvim | lua require('dotfiles.plugins').install()]])
vim.cmd([[command! PackerUpdate packadd packer.nvim | lua require('dotfiles.plugins').update()]])
vim.cmd([[command! PackerSync packadd packer.nvim | lua require('dotfiles.plugins').sync()]])
vim.cmd([[command! PackerClean packadd packer.nvim | lua require('dotfiles.plugins').clean()]])
vim.cmd([[command! PackerCompile packadd packer.nvim | lua require('dotfiles.plugins').compile()]])

-- Refresh packer lazy-loading when plugins.lua changes:
vim.cmd([[autocmd! dotfiles-settings BufWritePost plugins.lua source <afile> | PackerCompile]])

-- Install packer.nvim, if it isn't present:
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", vim.g.install_path })
	vim.cmd([[PackerSync]])
end
-- }}}
-- # vim:foldmethod=marker:foldlevel=0
