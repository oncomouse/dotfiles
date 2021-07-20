-- Dotfiles Settings: {{{
dotfiles = _G.dotfiles or {}
-- Taken from doom-nvim (NTBBloodbath/doom-nvim) {{{
-- Disable these for very fast startup time
vim.cmd([[
	syntax off
	filetype off
	filetype plugin indent off
]])
-- Temporarily disable shada file to improve performance
vim.opt.shadafile = 'NONE'
-- Disable some unused built-in Neovim plugins
vim.g.loaded_man = false
vim.g.loaded_gzip = false
vim.g.loaded_netrwPlugin = false
vim.g.loaded_tarPlugin = false
vim.g.loaded_zipPlugin = false
vim.g.loaded_2html_plugin = false
vim.g.loaded_remote_plugins = false
-- }}}

if vim.fn.empty(vim.fn.glob('~/dotfiles/conf/vim/')) == 0
	and not vim.tbl_contains(vim.opt.runtimepath:get(), vim.fn.expand('~/dotfiles/conf/vim')) then
	vim.opt.runtimepath:append("~/dotfiles/conf/vim")
end

-- XDG Helper:
local xdg = require("dotfiles.utils.xdg")

-- Don't use the minimal minpac install in vimrc-minimal
vim.g.skip_minimal_minpac = 1

-- Load Basic Settings:
vim.cmd[[ runtime vimrc-minimal ]]

-- Add Dotfiles After To RTP:
vim.opt.runtimepath:append("~/dotfiles/conf/vim/after/")

-- }}}
-- FZF Setup {{{
if vim.fn.isdirectory('/usr/local/opt/fzf') == 1 then
	vim.opt.runtimepath:append("/usr/local/opt/fzf")
elseif vim.fn.isdirectory('/usr/share/vim/vimfiles') == 1 then
	vim.opt.runtimepath:append("/usr/share/vim/vimfiles")
elseif vim.fn.isdirectory('~/.fzf') == 1 then
	vim.opt.runtimepath:append("~/.fzf")
end
-- }}}
-- Check if we can load submodules:
local stow_packpath = xdg("XDG_DATA_HOME") .. "/dotfiles/neovim"
if vim.fn.isdirectory(stow_packpath) == 1 then
	-- Add submodule directory (made using stow) to packpath:
	vim.opt.packpath:append(stow_packpath)
	-- Turn on options:
	vim.g.dotfiles_loaded_pack = 1
	local async

	async = vim.loop.new_async(vim.schedule_wrap(function()
		vim.defer_fn(function()
			vim.opt.shadafile = ''
			vim.defer_fn(function()
				vim.cmd[[
				rshada!
				doautocmd BufRead
				syntax on
				filetype on
				filetype plugin indent on
				silent! bufdo e
				]]
				-- Define autogroup {{{
				vim.cmd[[
				augroup dotfiles-settings
				autocmd!
				augroup END
				]]
				-- }}}
				-- Mac NeoVim Settings: {{{
				if vim.fn.has('mac') == 1 and vim.fn.has('nvim') == 1 then
					vim.g.python_host_prog='/usr/bin/python2.7'
					vim.g.python3_host_prog='/usr/local/bin/python3'
					vim.g.ruby_host_prog=vim.fn.expand('~/.asdf/shims/neovim-ruby-host')
					vim.g.node_host_prog='/usr/local/lib/node_modules/neovim/bin/cli.js'
					-- This is macOS only, I believe, but it fixes slow start-up for clipboard:
					vim.g.clipboard = {
						copy= {['+']= 'pbcopy', ['*']= 'pbcopy'},
						paste= {['+']= 'pbpaste', ['*']= 'pbpaste'},
						name= 'pbcopy', cache_enabled= 0
					}
				end
				-- }}}
				-- Dotfiles Settings: {{{
				-- Set Spellfile Location:
				vim.opt.spellfile = "~/dotfiles/conf/vim/spell/en.utf-8.add"

				-- Use g@ to capitalize words:
				vim.opt.operatorfunc="dotfiles#titlecase"
				-- }}}
				-- Tabs: {{{
				vim.opt.tabstop=4
				vim.opt.shiftwidth=4
				vim.opt.softtabstop=4
				vim.opt.expandtab = false
				-- }}}
				-- Folds: {{{
				vim.opt.foldmethod="syntax"
				vim.cmd[[
				autocmd dotfiles-settings FileType vim setlocal foldmethod=marker foldlevel=0
				autocmd dotfiles-settings FileType css setlocal foldmethod=syntax foldlevel=0
				autocmd dotfiles-settings FileType scss setlocal foldmethod=syntax foldlevel=0
				autocmd dotfiles-settings FileType python setlocal foldmethod=indent
				autocmd dotfiles-settings FileType diff setlocal nofoldenable
				]]
				-- }}}
				-- Theme: {{{
				-- Fancy color for macs and X11 sessions:
				if vim.fn.has("mac") == 1 or vim.fn.exists("$XAUTHORITY") == 1 then
					local t = require("dotfiles.utils.termcode")
					vim.cmd("let &t_8f='" .. t"<Esc>" .. "[38;2;%lu;%lu;%lum'")
					vim.cmd("let &t_8b='" .. t"<Esc>" .. "[48;2;%lu;%lu;%lum'")
					vim.opt.termguicolors = true

					local wal_cache = xdg("XDG_CACHE_HOME") .. '/wal/vim'
					if vim.fn.isdirectory(wal_cache) == 1 then
						vim.opt.runtimepath:append({ wal_cache })
						vim.cmd("colorscheme wal")
					else
						vim.cmd("colorscheme default")
					end
					else
					vim.cmd("colorscheme default")
				end
				-- }}}
				-- Other Settings: {{{

				-- Highlighted Yank:
				vim.cmd[[
				autocmd dotfiles-settings TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}
				]]

				-- Close Preview Window:
				vim.cmd[[
				autocmd dotfiles-settings CompleteDone * if pumvisible() == 0 | pclose | endif
				]]

				-- On opening a file, jump to the last known cursor position (see :h line())
				function dotfiles.buf_jump_to_last()
					if vim.fn.line("'\"") > 1
					and vim.fn.line("'\"") <= vim.fn.line("$")
					and string.find(vim.opt.filetype:get(), "commit") == nil then
						vim.cmd[[exe "normal! g`\""]]
					end
				end
				vim.cmd[[
				autocmd dotfiles-settings BufReadPost * lua dotfiles.buf_jump_to_last()
				]]

				-- Fix window resizing
				vim.cmd[[
				autocmd dotfiles-settings VimEnter * silent exec "!kill -s SIGWINCH $PPID"
				]]

				-- Update FASD For NeoVim: {{{
				function dotfiles.fasd_update()
					if #vim.opt.buftype:get() == 0 then
						local handle
						handle = vim.loop.spawn('fasd', {
							args = {
								'-A', vim.fn.expand('%:p'),
							}
						}, function() handle:close() end)
					end
				end
				vim.cmd[[
				autocmd dotfiles-settings BufWinEnter,BufFilePost * call s:fasd_update()
				]]
				-- }}}
				-- }}}
				require("dotfiles.settings")
			end, 0)
		end, 0)
		async:close()
	end))
	async:send()
end
-- }}}
-- # vim:foldmethod=marker
