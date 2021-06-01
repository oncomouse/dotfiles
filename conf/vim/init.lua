-- Dotfiles Settings: {{{
if vim.fn.isdirectory('~/dotfiles/conf/vim/') == 0 and
not vim.tbl_contains(vim.opt.runtimepath:get(), vim.fn.expand("~/dotfiles/conf/vim/")) then
	vim.opt.runtimepath:append("~/dotfiles/conf/vim/")
end

-- Don't use the minimal minpac install in vimrc-minimal
vim.g.skip_minimal_minpac = 1

-- Load Basic Settings:
vim.cmd("runtime vimrc-minimal")

-- Add Dotfiles After To RTP:
vim.opt.runtimepath:append("~/dotfiles/conf/vim/after/")

-- Set Spellfile Location:
vim.opt.spellfile="~/dotfiles/conf/vim/spell/en.utf-8.add"

-- Statusline:
function _G.SL_WC()
	if vim.fn.matchstr(vim.opt.filetype:get(), "\\v^(markdown|text|vimwiki)") ~= 0 then
		return ' W:' .. vim.fn.wordcount().words
	end
	return ''
end

function _G.sl_dg()
	local d = ''
	for kind,marker in pairs({ Error = " E:", Warning = " W:", Information = " I:", Hint = " H:" }) do
		local c = vim.lsp.diagnostic.get_count(0, kind)
		if c ~= 0 then
			d = d .. marker .. tostring(c)
		end
	end
	return d
end

local statusline = ' %0.45f%m%h%w%r%= %y%{v:lua.SL_WC()} %l:%c%{v:lua.sl_dg()} '
local statusline_nc = ' %0.45f%m%h%w%r%='
function _G.SL_STL()
	return vim.g.statusline_winid == vim.fn.win_getid() and statusline or statusline_nc
end
vim.opt.statusline="%!v:lua.SL_STL()"

-- Use g@ to capitalize words:
vim.opt.operatorfunc="dotfiles#titlecase"
-- }}}
-- Mac NeoVim Settings: {{{
if vim.fn.has('mac') == 1 then
	vim.g.python_host_prog='/usr/bin/python2.7'
	vim.g.python3_host_prog='/usr/local/bin/python3'
	vim.g.ruby_host_prog=vim.fn.expand('~/.asdf/shims/neovim-ruby-host')
	vim.g.node_host_prog='/usr/local/lib/node_modules/neovim/bin/cli.js'
	vim.g.clipboard = {
		copy = {["+"] = 'pbcopy', ["*"] = 'pbcopy'},
		paste = {["+"] = 'pbpaste', ["*"] = 'pbpaste'},
		name = 'pbcopy', cache_enabled = 0
	}
end
-- }}}
-- Autogroups {{{
vim.cmd[[augroup dotfiles-settings
	autocmd!
augroup END
augroup plug-settings
	autocmd!
augroup END]]
-- }}}
-- Tabs: {{{
vim.opt.tabstop=4
vim.opt.shiftwidth=4
vim.opt.softtabstop=4
vim.opt.expandtab = false
-- }}}
-- Folds: {{{
vim.opt.foldmethod="syntax"
vim.cmd[[autocmd dotfiles-settings FileType vim setlocal foldmethod=marker foldlevel=0
autocmd dotfiles-settings FileType css setlocal foldmethod=syntax foldlevel=0
autocmd dotfiles-settings FileType scss setlocal foldmethod=syntax foldlevel=0
autocmd dotfiles-settings FileType python setlocal foldmethod=indent
autocmd dotfiles-settings FileType diff setlocal nofoldenable]]
-- }}}
-- Plugins: {{{
require("dotfiles.packer")
-- }}}
-- Maps: {{{
require("dotfiles.maps")
-- }}}
-- Theme: {{{
local t = require("dotfiles.utils.termcode")
vim.cmd("let &t_8f='" .. t"<Esc>" .. "[38;2;%lu;%lu;%lum'")
vim.cmd("let &t_8b='" .. t"<Esc>" .. "[48;2;%lu;%lu;%lum'")
vim.opt.termguicolors = true
vim.opt.background = "dark"

local xdg = function(var_name)
	if var_name == "XDG_CACHE_HOME" then
		return os.getenv("XDG_CACHE_HOME") and os.getenv("XDG_CACHE_HOME") or os.getenv("HOME") .. '/.cache'
	end
	return nil
end

local wal_cache = xdg("XDG_CACHE_HOME") .. '/wal/vim'
if vim.fn.isdirectory(wal_cache) == 1 then
	vim.opt.runtimepath:append({ wal_cache })
	vim.cmd("colorscheme wal")
else
	vim.cmd("colorscheme default")
end

-- }}}
-- Other Settings: {{{

-- Highlighted Yank:
vim.cmd("autocmd plug-settings TextYankPost * silent! lua vim.highlight.on_yank {higroup=\"IncSearch\", timeout=500}")

-- Close Preview Window:
vim.cmd("autocmd dotfiles-settings CompleteDone * if pumvisible() == 0 | pclose | end")

-- On opening a file, jump to the last known cursor position (see :h line())
function _G.buf_jump_to_last()
	if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") and string.find(vim.opt.filetype:get(), "commit") == nil then
		vim.cmd[[exe "normal! g`\""]]
	end
end
vim.cmd("autocmd dotfiles-settings BufReadPost * call v:lua.buf_jump_to_last()")

-- Fix window resizing
vim.cmd("autocmd dotfiles-settings VimEnter * silent exec \"!kill -s SIGWINCH $PPID\"")

-- Update FASD For NeoVim: {{{
function _G.fasd_update()
	if #vim.opt.buftype:get() == 0 then
		local handle
		handle = vim.loop.spawn('fasd', {
			args = {
				'-A', vim.fn.expand('%:p'),
			}
		}, function() handle:close() end)
	end
end
vim.cmd("autocmd dotfiles-settings BufWinEnter,BufFilePost * call v:lua.fasd_update()")
-- }}}
-- }}}
-- # vim:foldmethod=marker
