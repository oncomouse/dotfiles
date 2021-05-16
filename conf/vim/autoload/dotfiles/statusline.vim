" Statusline:
scriptencoding utf-8
let g:statusline_soft_sep = get(g:, 'statusline_soft_sep', '⋮')
" Linter Status {{{
	let g:dotfiles#diagnostics#indicator_checking = ''
	let g:dotfiles#diagnostics#indicator_warnings = ' W:'
	let g:dotfiles#diagnostics#indicator_errors = ' E:'
	let g:dotfiles#diagnostics#indicator_information = ' I:'
	let g:dotfiles#diagnostics#indicator_ok = ''
" }}}
" Statusline {{{
	function! dotfiles#statusline#componetize(func,...) abort
		let l:before = get(a:, 1, ' ')
		let l:after = get(a:, 2, ' ')
		let l:output = eval(a:func)
		if l:output ==# ''
			return ''
		endif
		return l:before.l:output.l:after
	endfunction
	" Based on: https://github.com/lifepillar/vimrc
	let g:lf_stlh = {
		\ 'n': 'NormalMode', 'i': 'InsertMode', 'R': 'ReplaceMode',
		\ 'v': 'VisualMode', 'V': 'VisualMode', "\<c-v>": 'VisualMode',
		\ 's': 'VisualMode', 'S': 'VisualMode', "\<c-s>": 'VisualMode',
		\ 'c': 'CommandMode', 'r': 'CommandMode', 't': 'TerminalMode',
		\ '!': 'CommandMode', '': 'StatusLineNC'
		\ 
		\}
	let g:lf_stlm = {
		\ 'n': 'Normal', 'i': 'Insert', 'R': 'Replace',
		\ 'v': 'Visual', 'V': 'V·Line', "\<c-v>": 'V·Block',
		\ 's': 'Select', 'S': 'S·Line', "\<c-s>": 'S·Block',
		\ 'c': 'Command', 'r': 'Prompt', 't': 'Terminal',
		\ '!': 'Shell'}
	" curwin is always the number of the currently active window. In a %{}
	" context, winnr() always refers to the window to which the status line
	" being drawn belongs. Since this function is invoked in a %{} context,
	" winnr() may be different from a:curwin. We use this fact to detect
	" whether we are drawing in the active window or in an inactive window.
	function! dotfiles#statusline#wordcount() abort
		return &filetype =~# '\v^(markdown|txt|vimwiki)' ? ' W:' . wordcount().words : ''
	endfunction
	function! dotfiles#statusline#mode() abort
		if &filetype ==# 'gina-status'
			return 'Gina'
		endif
		if &filetype ==# 'qf'
			return ''
		endif
		return get(g:lf_stlm, mode())
	endfunction
	function! dotfiles#statusline#statusline() abort
		return ' %0.45f%m%h%w%r%= %y
			\%{dotfiles#statusline#wordcount()}
			\ %l/%L:%c
			\%{dotfiles#diagnostics#warnings() }
			\%{dotfiles#diagnostics#errors() }
			\%* '
	endfunction
" }}}
