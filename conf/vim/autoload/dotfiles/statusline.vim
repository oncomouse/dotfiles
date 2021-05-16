" Statusline:
scriptencoding utf-8
let g:statusline_soft_sep = get(g:, 'statusline_soft_sep', '⋮')
" Linter Status {{{
	let g:dotfiles#diagnostics#indicator_checking = ''
	let g:dotfiles#diagnostics#indicator_warnings = ''
	let g:dotfiles#diagnostics#indicator_errors = ''
	let g:dotfiles#diagnostics#indicator_information = ''
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
		return &filetype =~# '\v^(markdown|txt|vimwiki)' ? wordcount().words . ' words ' : ''
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
		if g:statusline_winid == win_getid(winnr())
			return '%#'.get(g:lf_stlh, mode()).'#'.
				\"%{dotfiles#statusline#componetize('dotfiles#statusline#mode()', '  ', ' ')}
				\%1* %0.45f%m%h%w%r%=
				\ %y 
				\%#StatusLineInfo#
				\%{dotfiles#statusline#componetize('dotfiles#statusline#wordcount()', ' ', g:statusline_soft_sep)}
				\ %l/%L:%c 
				\%#StatusWarning#%{dotfiles#statusline#componetize('dotfiles#diagnostics#warnings()') }
				\%#StatusError#%{dotfiles#statusline#componetize('dotfiles#diagnostics#errors()') }
				\%#StatusOk#%{dotfiles#statusline#componetize('dotfiles#diagnostics#ok()') }
				\%#StatusWarning#%{dotfiles#statusline#componetize('dotfiles#diagnostics#checking()') }
				\%*"
		else
			return '%1* %0.45f%m%h%w%r%='
		endif
	endfunction
" }}}
