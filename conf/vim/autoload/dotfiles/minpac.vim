function! s:packpath() abort
	return ($XDG_DATA_HOME ? $XDG_DATA_HOME : $HOME.'/.local/share') . '/minpac/desktop'
endfunction

function! dotfiles#minpac#init() abort
	let l:minpac_dir = s:packpath()

	" Download Minpac:
	if empty(glob(l:minpac_dir.'/pack/minpac/opt/minpac'))
		if executable('git')
			silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.l:minpac_dir.'/pack/minpac/opt/minpac"'
		endif
	endif

	" Load Minpac:
	packadd minpac

	if exists('g:loaded_minpac')
		call minpac#init({'dir': l:minpac_dir})

		" Manage Minpac:
		call minpac#add('k-takata/minpac', {'type': 'opt'})
		" Basics:
		call minpac#add('tpope/vim-sensible') " Agreeable vim settings:
		call minpac#add('xero/securemodelines') " Secure modelines
		call minpac#add('oncomouse/vim-grep') " :Grep and :LGrep
		call minpac#add('tpope/vim-commentary') " Comment with gc<motion>
		" General Editing:
		call minpac#add('sickill/vim-pasta') " Indentation-forward pasting
		call minpac#add('tpope/vim-repeat')
		call minpac#add('oncomouse/vim-surround') " ys to add, cs to change, ds to delete. f, F for function, t, T for tag
		call minpac#add('airblade/vim-rooter') " Set project root
		call minpac#add('Konfekt/FastFold') " Better fold support
		call minpac#add('wellle/targets.vim') " add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
		call minpac#add('cohama/lexima.vim') " Autopairs + Endwise
		call minpac#add('norcalli/nvim-colorizer.lua') " HTML codes and HTML color words to colors
		call minpac#add('windwp/nvim-ts-autotag', { 'type': 'opt', 'branch': 'main' }) " Automatically close HTML tags
		" Git Support:
		call minpac#add('lambdalisue/gina.vim') " :Gina status to schedule; :Gina commit to commit
		" FZF Support:
		call minpac#add('junegunn/fzf.vim', { 'type': 'opt' }) " Add shorcuts for FZF
		call minpac#add('gfanto/fzf-lsp.nvim', {  'type': 'opt', 'branch': 'main' })
		" Syntax:
		call minpac#add('nvim-treesitter/nvim-treesitter')
		call minpac#add('nvim-treesitter/nvim-treesitter-textobjects')
		call minpac#add('plasticboy/vim-markdown') " Markdown Syntax
		call minpac#add('godlygeek/tabular') " :Tabular /| to auto-align tables (also :TableFormat in markdown)
		call minpac#add('cakebaker/scss-syntax.vim') " SCSS Syntax
		call minpac#add('oncomouse/vim-fish') " Fish Syntax & Async Completion
		" LSP:
		call minpac#add('neovim/nvim-lspconfig') " LSP Configuration
	else
		echoerr "Could not load minpac. Perhaps your Internet is not working or you don't have git?"
	endif
endfunction


function! dotfiles#minpac#configuration() abort
	" Add Install Dir To Path:
	exe 'set packpath+='.s:packpath()

	" Minpac Commands:
	command! PackUpdate call dotfiles#minpac#init() | call minpac#update()
	command! PackClean call dotfiles#minpac#init() | call minpac#clean()
	command! PackStatus call dotfiles#minpac#init() | call minpac#status()

	" Load Plugins:
	packloadall

	" Plugin Settings: {{{
	" airblade/vim-rooter 
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
	" cohama/lexima.vim 
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
	let g:fastfold_savehook = 1
	let g:fastfold_fold_command_suffixes =	['x','X','a','A','o','O','c','C', 'r', 'R', 'm', 'M']
	let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']
	let g:fastfold_minlines = 0
	" windwp/nvim-ts-autotag 
	autocmd plug-settings FileType html,javascript,javascriptreact packadd nvim-ts-autotag
	" lambdalisue/gina.vim 
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
	cnoreabbrev gina Gina
	" junegunn/fzf 
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
		packadd fzf-lsp.nvim
	endif
	" plasticboy/vim-markdown 
	let g:vim_markdown_frontmatter = 1 " Format YAML
	let g:vim_markdown_strikethrough = 0 " Don't format strikethrough
	let g:vim_markdown_conceal = 0 " Don't conceal
	let g:vim_markdown_conceal_code_blocks = 0 " Don't conceal code blocks
	let g:vim_markdown_math = 1 " Do process MathJaX and LaTeX math
	" }}}
endfunction
