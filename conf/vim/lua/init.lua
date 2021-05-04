-- Dotfiles Settings: {{{
vim.o.runtimepath=vim.o.runtimepath .. ',~/dotfiles/conf/vim/'

-- Load Basic Settings:
require('minimal')

-- Add Dotfiles After To RTP:
vim.o.runtimepath=vim.o.runtimepath..",~/dotfiles/conf/vim/after/"

-- Set Spellfile Location:
vim.bo.spellfile="~/dotfiles/conf/vim/spell/en.utf-8.add"

-- Statusline:
vim.o.statusline="%!dotfiles#statusline#statusline()"
-- }}}
-- Helper Functions {{{
local function xdg(var_name)
	if var_name == 'cache' then
		return os.getenv("XDG_CACHE_HOME") and os.getenv("XDG_CACHE_HOME") or os.getenv("HOME")..'/.cache'
	elseif var_name == 'data' then
		return os.getenv("XDG_DATA_HOME") and os.getenv("XDG_DATA_HOME") or os.getenv("HOME")..'/.local/share'
	elseif var_name == 'config' then
		return os.getenv("XDG_CONFIG_HOME") and os.getenv("XDG_CONFIG_HOME") or os.getenv("HOME")..'/.config'
	end
	return ''
end
-- Call t'<Space>''
local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end
-- }}}
-- Mac NeoVim Settings: {{{
if vim.fn.has('mac') == 1 and vim.fn.has('nvim') == 1 then
	vim.g.python_host_prog = '/usr/bin/python2.7'
	vim.g.python3_host_prog = '/usr/local/bin/python3'
	vim.g.ruby_host_prog = vim.fn.expand('~/.asdf/shims/neovim-ruby-host')
	vim.g.node_host_prog = '/usr/local/lib/node_modules/neovim/bin/cli.js'
	-- This is macOS only, I believe, but it fixes slow start-up for clipboard:
	vim.g.clipboard = {
	copy = {['+'] = 'pbcopy', ['*'] = 'pbcopy'},
	paste = {['+'] = 'pbpaste', ['*'] = 'pbpaste'},
	name = 'pbcopy', cache_enabled = 0
	}
end
-- }}}
-- Autogroups {{{
vim.api.nvim_command[[augroup dotfiles-settings]]
vim.api.nvim_command[[autocmd!]]
vim.api.nvim_command[[augroup END]]
vim.api.nvim_command[[augroup plug-settings]]
vim.api.nvim_command[[autocmd!]]
vim.api.nvim_command[[augroup END]]
-- }}}
-- Tabs: {{{
vim.bo.tabstop=4
vim.bo.shiftwidth=4
vim.bo.softtabstop=4
vim.bo.expandtab=false
-- }}}
-- Folds: {{{
vim.wo.foldmethod="syntax"
vim.api.nvim_command[[autocmd dotfiles-settings FileType vim setlocal foldmethod=marker foldlevel=0
autocmd dotfiles-settings FileType css setlocal foldmethod=syntax foldlevel=0
autocmd dotfiles-settings FileType scss setlocal foldmethod=syntax foldlevel=0
autocmd dotfiles-settings FileType python setlocal foldmethod=indent
autocmd dotfiles-settings FileType diff setlocal nofoldenable]]
-- }}}
-- Plugins: {{{
-- Configure paq: {{{
local pack_path_addition = 	xdg('data') .. '/paq'
vim.o.packpath = vim.o.packpath .. ',' .. pack_path_addition
vim.o.runtimepath = vim.o.runtimepath .. ',' .. pack_path_addition
function _G.pack_init()
	-- Download Minpac:
	-- local paq_dir = vim.fn.stdpath('data') .. '/site/pack/paq'
	local paq_dir = pack_path_addition .. '/pack/paq'
	if vim.fn.isdirectory(paq_dir) == 0 then
		vim.fn.system('git clone --depth 1 https://github.com/savq/paq-nvim "'..paq_dir..'/opt/paq-nvim"')
	end

	-- Load Minpac:
	vim.api.nvim_command("packadd paq-nvim")
	local paq = require'paq-nvim'.paq
	require'paq-nvim'.setup({ path=paq_dir..'/' })
	paq({'savq/paq-nvim', opt=true})
	paq('tpope/vim-sensible') -- Agreeable vim settings:
	paq('xero/securemodelines') -- Secure modelines
	paq('oncomouse/vim-grep') -- :Grep and :LGrep
	paq('tpope/vim-commentary')
	paq('sickill/vim-pasta') -- Indentation-forward pasting
	paq('tpope/vim-repeat')
	paq('oncomouse/vim-surround') -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
	paq('airblade/vim-rooter') -- Set project root
	paq('wellle/targets.vim') -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
	paq('cohama/lexima.vim') -- Autopairs + Endwise
	paq('Konfekt/FastFold') -- Better fold support
	paq('lambdalisue/gina.vim') -- :Gina status to schedule; :Gina commit to commit
	paq('norcalli/nvim-colorizer.lua') -- HTML codes and HTML color words to colors
	paq('nvim-treesitter/nvim-treesitter')
	paq('nvim-treesitter/nvim-treesitter-textobjects')
	paq('windwp/nvim-ts-autotag') -- Auotmatically close HTML tags
	paq('plasticboy/vim-markdown') -- Markdown Syntax
	paq('cakebaker/scss-syntax.vim') -- SCSS Syntax
	paq('oncomouse/vim-fish') -- Fish Syntax & Async Completion
	paq('neovim/nvim-lspconfig')
	paq({'junegunn/fzf.vim', opt=true}) -- Add shorcuts for FZF
	paq({'gfanto/fzf-lsp.nvim', opt=true})
	paq('godlygeek/tabular') -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
	paq({'kana/vim-textobj-user', opt=true}) -- Allow custom textobj definitions
	paq({'reedes/vim-textobj-sentence', opt=true}) -- Use as & is for selecting sentences; g) and g( for moving
	paq({'reedes/vim-textobj-quote', opt=true}) -- Makes aq & iq for smart quotes
end
vim.api.nvim_command("command! PackInstall call v:lua.pack_init() | lua require('paq-nvim').install()")
vim.api.nvim_command("command! PackUpdate call v:lua.pack_init() | lua require('paq-nvim').update()")
vim.api.nvim_command("command! PackClean call v:lua.pack_init() | lua require('paq-nvim').clean()")
vim.api.nvim_command("command! PackStatus call v:lua.pack_init() | lua require('paq-nvim').list()")
vim.api.nvim_command("packloadall")
-- }}}
-- Paq Configuration: {{{
-- airblade/vim-rooter {{{
vim.g.rooter_patterns = {
	'Rakefile',
	'package.json',
	'.git/',
	'Gemfile',
	'pyproject.toml',
	'setup.py',
}
-- Set path expansion to pwd only, especially with vim-rooter running:
vim.o.path=",,"
-- }}}
-- cohama/lexima.vim {{{
local function make_rule(at, ed, filetype, syntax)
	return {
		char = '<CR>',
		input = '<CR>',
		input_after = '<CR>' .. ed,
		at = at,
		except = '\\C\\v^(\\s*)\\S.*%#\\n%(%(\\s*|\\1\\s.+)\\n)*\\1' .. ed,
		filetype = filetype,
		syntax = syntax,
	}
end
function _G.extend_endwise()
	-- Lua endwise rules:
	vim.fn['lexima#add_rule'](make_rule('^\\s*if\\>.*then\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#', 'end', 'lua', { }))
	vim.fn['lexima#add_rule'](make_rule('^\\s*\\%(for\\|while\\)\\>.*do\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#', 'end', 'lua', { }))
	vim.fn['lexima#add_rule'](make_rule('^\\s*\\%(local\\)\\=.*function\\>\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#', 'end', 'lua', { }))
end
vim.api.nvim_command[[autocmd plug-settings VimEnter * call v:lua.extend_endwise()]]
-- vim.api.nvim_set_keymap('i', '<C-l>', '<C-r>=lexima#insmode#leave_till_eol("")<CR>', { noremap = true })
-- }}}
-- Konfekt/FastFold {{{
vim.g.fastfold_savehook = 1
vim.g.fastfold_fold_command_suffixes = {'x','X','a','A','o','O','c','C', 'r', 'R', 'm', 'M'}
vim.g.fastfold_fold_movement_commands = {']z', '[z', 'zj', 'zk'}
vim.g.fastfold_minlines = 0
-- }}}
-- gina.vim {{{
function _G.load_gina()
	vim.fn['gina#custom#command#option']('status', '--opener', vim.o.previewheight .. 'split')
	vim.fn['gina#custom#command#option']('commit', '--opener', vim.o.previewheight .. 'split')
	vim.fn['gina#custom#command#option']('diff', '--opener', vim.o.previewheight .. 'split')
	vim.fn['gina#custom#command#option']('status', '--group', 'short')
	vim.fn['gina#custom#command#option']('commit', '--group', 'short')
	vim.fn['gina#custom#command#option']('diff', '--group', 'short')
	-- Implement vim-fugitive commands in Gina:
	vim.fn['gina#custom#mapping#nmap']('status', 'cc', ':<C-u>Gina commit<CR>', {noremap = 1, silent = 1})
end
vim.api.nvim_command [[ autocmd plug-settings VimEnter * call v:lua.load_gina() ]]
vim.api.nvim_command [[ cnoreabbrev gina Gina ]]
-- }}}
-- plasticboy/vim-markdown {{{
vim.g.vim_markdown_frontmatter = 1 -- Format YAML
vim.g.vim_markdown_strikethrough = 0 -- Don't format strikethrough
vim.g.vim_markdown_conceal = 0 -- Don't conceal
vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
-- }}}
-- junegunn/fzf.vim {{{
if vim.fn.executable('fzf') == 1 then
	-- macOS Homebrew
	if vim.fn.isdirectory('/usr/local/opt/fzf') == 1 then
		vim.o.runtimepath=vim.o.runtimepath..",/usr/local/opt/fzf"
	-- Arch
	elseif vim.fn.isdirectory('/usr/share/vim/vimfiles') == 1 then
		vim.o.runtimepath=vim.o.runtimepath..",/usr/share/vim/vimfiles"
	-- Local install
	elseif vim.fn.isdirectory('~/.fzf') == 1 then
		vim.o.runtimepath=vim.o.runtimepath..",~/.fzf"
	end
end
-- }}}
-- Writing: {{{
vim.g['textobj#quote#educate'] = 0
-- Initialize the plugin when it is dynamically loaded:
vim.api.nvim_command [[
	autocmd plug-settings FileType markdown,text,mail packadd vim-textobj-user | packadd vim-textobj-sentence | packadd vim-textobj-quote
	autocmd plug-settings User vim-textobj-sentence call textobj#sentence#init()
	autocmd plug-settings User vim-textobj-quote call textobj#quote#init()
]]
-- }}}
-- }}}
-- }}}
-- Maps: {{{
vim.g.dwm_map_keys = 0
vim.g.dwm_load = 0
-- Highlight a block and type --@" to run a macro on the block:
vim.api.nvim_set_keymap('x', '@', ':<C-u>call visualat#execute_macro_over_visual_range()<CR>', { silent=true, noremap = true })
-- Update fast folds:
vim.api.nvim_set_keymap('n', 'zuz', '<Plug>(FastFoldUpdate)', {})
-- Grep project:
function _G.grep_or_qfgrep()
	if vim.bo.buftype ==# 'quickfix' then
		local input = vim.fn.input('QFGrep/')
		if string.len(input) > 0 then
			vim.api.nvim_exec('Cfilter /' .. input .. '/')
		end
	else
		local input = vim.fn.input('Grep/')
		if string.len(input) > 0 then
			vim.api.nvim_exec('Grep ' .. input)
		end
	end
end
vim.api.nvim_set_keymap('n', '<leader>/', '<cmd>call v:lua.grep_or_qfgrep()<CR>', { silent = true, noremap = true })
-- Calculator (not sure how this works):
vim.api.nvim_set_keymap('i', '<C-A>', '<C-O>yiW<End>=<C-R>=<C-R>0<CR>', { noremap = true })
-- Shortcut to view current syntax highlighting group:
vim.api.nvim_set_keymap('n', '<F10>', [[:echo --hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
	\ . synIDattr(synID(line("."),col("."),0),"name") . --> lo<"
	\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . -->"<CR>]], {})
-- List Bindings: {{{
vim.api.nvim_set_keymap('n', '<leader>d', ':call dotfiles#lists#toggle(\'Location List\', \'l\')<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>q', ':call dotfiles#lists#toggle(\'Quickfix List\', \'c\')<CR>', { silent = true, noremap = true })
--}}}
-- Default Bindings: {{{
vim.api.nvim_set_keymap('n', '<Plug>(dotfiles-diagnostic-next)', '<cmd>cnext<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Plug>(dotfiles-diagnostic-previous)', '<cmd>cprev<CR>', { silent = true })
-- }}}
-- Standard Fuzzy Bindings: {{{
vim.api.nvim_set_keymap('n', '<c-p>', '<Plug>(dotfiles-files)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>F', '<Plug>(dotfiles-home-files)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>a', '<Plug>(dotfiles-buffers)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>A', '<Plug>(dotfiles-windows)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>l', '<Plug>(dotfiles-lines)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>?', '<Plug>(dotfiles-commands)', { silent = true })
-- }}}
-- Standard LSP Bindings: {{{
-- As with Fuzzy bindings (above), we set all the LSP commands to Plug
-- bindings and then rebind them here to the keys we actually want to use:
vim.api.nvim_set_keymap('n', '<F2>', '<Plug>(dotfiles-rename)', { silent = true })
vim.api.nvim_set_keymap('n', '<F5>', '<Plug>(dotfiles-commands)', { silent = true })
vim.api.nvim_set_keymap('v', 'ga', '<Plug>(dotfiles-codeaction-selected)', { silent = true })
vim.api.nvim_set_keymap('n', 'ga', '<Plug>(dotfiles-codeaction)', { silent = true })
vim.api.nvim_set_keymap('n', 'gl', '<Plug>(dotfiles-codelens)', { silent = true })
vim.api.nvim_set_keymap('n', 'gd', '<Plug>(dotfiles-definition)', { silent = true })
vim.api.nvim_set_keymap('n', 'gy', '<Plug>(dotfiles-type-definition)', { silent = true })
vim.api.nvim_set_keymap('n', 'gi', '<Plug>(dotfiles-implementation)', { silent = true })
vim.api.nvim_set_keymap('n', 'gr', '<Plug>(dotfiles-references)', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>s', '<Plug>(dotfiles-document-symbols)', { silent = true })
vim.api.nvim_set_keymap('n', 'K', '<Plug>(dotfiles-documentation)', { silent = true })
vim.api.nvim_set_keymap('n', '[d', '<Plug>(dotfiles-diagnostic-previous)', { silent = true })
vim.api.nvim_set_keymap('n', ']d', '<Plug>(dotfiles-diagnostic-next)', { silent = true })
-- }}}
-- }}}
-- Theme: {{{
vim.o.background="dark"
-- Fancy Colors for Desktop Mode:
if vim.fn.exists('+termguicolors') == 1 then
	vim.o.t_8f = "\\<Esc>[38;2;%lu;%lu;%lum"
	vim.o.t_8b = "\\<Esc>[48;2;%lu;%lu;%lum"
	vim.o.termguicolors=true
	-- nvim-colorizer:
	if vim.fn.has('nvim') == 1 then
		require'colorizer'.setup{
			'*',
			markdown={
				names=false
			},
			text={
				names=false
			}
		}
	end
end
vim.o.runtimepath=vim.o.runtimepath..','..vim.fn.expand(xdg('cache') .. '/wal/vim')
vim.api.nvim_command('colorscheme wal')
-- }}}
-- Other Settings: {{{
-- Configure Treesitter:
require('dotfiles.treesitter')
-- Highlighted Yank:
vim.api.nvim_command[[autocmd plug-settings TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}]]
-- Close preview window:
vim.api.nvim_command[[ autocmd dotfiles-settings CompleteDone * if pumvisible() == 0 | pclose | end ]]
-- On opening a file, jump to the last known cursor position (see :h line())
vim.api.nvim_command[[ autocmd dotfiles-settings BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit' |
\	 exe --normal! g`\"" |
\ end ]]
-- Load Autocompletion: {{{
if vim.fn.executable('fzf') == 1 then
	-- Load FZF plugins:
	vim.api.nvim_command("packadd fzf.vim")
	vim.api.nvim_command("packadd fzf-lsp.nvim")
	vim.fn["dotfiles#autocomplete#fzf#init"]()
end
require('dotfiles.nvim_lsp')
-- }}}
-- Update FASD For NeoVim: {{{
function _G.fasd_update()
	if vim.fn.empty(vim.bo.buftype) == 1 then
		handle = vim.loop.spawn('fasd', {
			args={'-A', vim.fn.expand('%:p') },
		}, function() handle:close() end)
	end
end
vim.api.nvim_command[[ autocmd dotfiles-settings BufWinEnter,BufFilePost * call v:lua.fasd_update() ]]
-- }}}
-- }}}
-- # vim:foldmethod=marker
