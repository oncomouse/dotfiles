function! s:packpath() abort
	return ($XDG_DATA_HOME ? $XDG_DATA_HOME : $HOME.'/.local/share') . '/paq/desktop'
endfunction

lua << EOF
function _G.pack_init()
	-- Download Paq:
	local paq_dir = vim.fn["s:packpath"]() .. '/pack/paq'
	if vim.fn.isdirectory(paq_dir) == 0 then
		vim.fn.system('git clone --depth 1 https://github.com/savq/paq-nvim "'..paq_dir..'/opt/paq-nvim"')
	end

	-- Load Minpac:
	vim.api.nvim_command("packadd paq-nvim")
	local paq = require'paq-nvim'.paq
	require'paq-nvim'.setup{ path=paq_dir..'/' }

	-- Manage Paq:
	paq{ 'savq/paq-nvim', opt=true }
	-- Getting Started:
	paq'tpope/vim-sensible' -- Agreeable vim settings:
	paq'xero/securemodelines' -- Secure modelines
	paq'oncomouse/vim-grep' -- :Grep and :LGrep
	paq'tpope/vim-commentary' -- gc<motion> to (un)comment
	-- General Editing:
	paq'sickill/vim-pasta' -- Indentation-forward pasting
	paq'tpope/vim-repeat'
	paq'oncomouse/vim-surround' -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
	paq'airblade/vim-rooter' -- Set project root
	paq'Konfekt/FastFold' -- Better fold support
	paq'wellle/targets.vim' -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
	paq'cohama/lexima.vim' -- Autopairs + Endwise
	paq{ 'norcalli/nvim-colorizer.lua', opt = true } -- HTML codes and HTML color words to colors
	paq{ 'windwp/nvim-ts-autotag', opt = true }  -- Automatically close HTML tags
	-- Git Support:
	paq'lambdalisue/gina.vim' -- :Gina status to schedule; :Gina commit to commit
	-- FZF Support:
	paq{ 'junegunn/fzf.vim', opt = true }  -- Add shorcuts for FZF
	-- Syntax:
	paq'nvim-treesitter/nvim-treesitter'
	paq'nvim-treesitter/nvim-treesitter-textobjects'
	paq'plasticboy/vim-markdown' -- Markdown Syntax
	paq'godlygeek/tabular' -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
	paq'cakebaker/scss-syntax.vim' -- SCSS Syntax
	paq'oncomouse/vim-fish' -- Fish Syntax & Async Completion
	-- LSP:
	paq'neovim/nvim-lspconfig' -- LSP Configuration
end
EOF

function! dotfiles#plugins#configuration() abort
	" Add Install Dir To Path:
	exe 'set packpath+='.s:packpath()

	" Minpac Commands:
	command! PackInstall call v:lua.pack_init() | lua require('paq-nvim').install()
	command! PackUpdate call v:lua.pack_init() | lua require('paq-nvim').update()
	command! PackClean call v:lua.pack_init() | lua require('paq-nvim').clean()
	command! PackStatus call v:lua.pack_init() | lua require('paq-nvim').list()

	" Load Plugins:
	packloadall

	" Plugin Settings: {{{
	" airblade/vim-rooter {{{ 
	let g:rooter_patterns = [
	\ 'Rakefile',
	\ 'package.json',
	\ '.git/',
	\ 'Gemfile',
	\ 'pyproject.toml',
	\ 'setup.py',
	\ ]
	" Set path expansion to pwd only, especially with vim-rooter running:
	set path=,,
	" }}}
	" cohama/lexima.vim{{{ 
	function! s:make_rule(at, end, filetype, syntax)
		return {
					\ 'char': '<CR>',
					\ 'input': '<CR>',
					\ 'input_after': '<CR>' . a:end,
					\ 'at': a:at,
					\ 'except': '\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1' . a:end,
					\ 'filetype': a:filetype,
					\ 'syntax': a:syntax,
					\ }
	endfunction
	function! s:extend_endwise() abort
		" Lua endwise rules:
		call lexima#add_rule(s:make_rule('^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
		call lexima#add_rule(s:make_rule('^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
		call lexima#add_rule(s:make_rule('^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
	endfunction
	autocmd plug-settings VimEnter * call s:extend_endwise()
	" inoremap <C-l> <C-r>=lexima#insmode#leave_till_eol("")<CR>
	" }}}
	" Konfekt/FastFold {{{
	let g:fastfold_savehook = 1
	let g:fastfold_fold_command_suffixes =	['x','X','a','A','o','O','c','C', 'r', 'R', 'm', 'M']
	let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']
	let g:fastfold_minlines = 0
	" }}}
	" windwp/nvim-ts-autotag{{{ 
	autocmd plug-settings FileType html,javascript,javascriptreact packadd nvim-ts-autotag
	" }}}
	" lambdalisue/gina.vim {{{
	function! s:load_gina() abort
		call gina#custom#command#option('status', '--opener', &previewheight . 'split')
		call gina#custom#command#option('commit', '--opener', &previewheight . 'split')
		call gina#custom#command#option('diff', '--opener', &previewheight . 'split')
		call gina#custom#command#option('status', '--group', 'short')
		call gina#custom#command#option('commit', '--group', 'short')
		call gina#custom#command#option('diff', '--group', 'short')
		" Implement vim-fugitive commands in Gina:
		call gina#custom#mapping#nmap('status', 'cc', ':<C-u>Gina commit<CR>', {'noremap': 1, 'silent': 1})
	endfunction
	autocmd plug-settings VimEnter * call s:load_gina()
	" }}}
	" junegunn/fzf {{{
	if executable('fzf')
		" macOS Homebrew
		if isdirectory('/usr/local/opt/fzf')
			set runtimepath+=/usr/local/opt/fzf
		" Arch
		elseif isdirectory('/usr/share/vim/vimfiles')
			set runtimepath+=/usr/share/vim/vimfiles
		" Local install
		elseif isdirectory('~/.fzf')
			set runtimepath+=~/.fzf
		endif
		packadd fzf.vim
	endif
	" }}}
	" plasticboy/vim-markdown {{{
	let g:vim_markdown_frontmatter = 1 " Format YAML
	let g:vim_markdown_strikethrough = 0 " Don't format strikethrough
	let g:vim_markdown_conceal = 0 " Don't conceal
	let g:vim_markdown_conceal_code_blocks = 0 " Don't conceal code blocks
	let g:vim_markdown_math = 1 " Do process MathJaX and LaTeX math
	" }}}
	" }}}
endfunction
