" Statusline:
scriptencoding utf-8
let g:nerdfonts = g:dotfiles_mode ==# 'desktop'
let g:statusline_left_sep = get(g:, 'statusline_left_sep', '') " g:nerdfonts ? '' :
let g:statusline_left_soft_sep = get(g:, 'statusline_left_soft_sep','│') "  g:nerdfonts ? '' : 
let g:statusline_right_sep = get(g:, 'statusline_right_sep',  '') " g:nerdfonts ? '' :
let g:statusline_right_soft_sep = get(g:, 'statusline_right_soft_sep', '│') " g:nerdfonts ? '' : 
" Linter Status {{{
  let g:dotfiles#ale#indicator_checking = g:nerdfonts ? "\uf110" : '…'
  let g:dotfiles#ale#indicator_warnings = g:nerdfonts ? "\uf071\u2003" : 'W: '
  let g:dotfiles#ale#indicator_errors = g:nerdfonts ? "\uf05e\u2003" : 'E: '
  let g:dotfiles#ale#indicator_information = g:nerdfonts ? "\uf7fc\u2003" : 'I: '
  let g:dotfiles#ale#indicator_ok = ''
" }}}
" Statusline {{{
  function! Componetize(func,...) abort
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
    \ 'n': 'NormalMode',  'i': 'InsertMode',      'R': 'ReplaceMode',
    \ 'v': 'VisualMode',  'V': 'VisualMode', "\<c-v>": 'VisualMode',
    \ 's': 'VisualMode',  'S': 'VisualMode', "\<c-s>": 'VisualMode',
    \ 'c': 'CommandMode', 'r': 'CommandMode',     't': 'TerminalMode',
    \ '!': 'CommandMode',  '': 'StatusLineNC'
    \ 
    \}
  let g:lf_stlm = {
    \ 'n': 'Normal',           'i': 'Insert',          'R': 'Replace',
    \ 'v': 'Visual',           'V': 'V·Line',          "\<c-v>": 'V·Block',
    \ 's': 'Select',           'S': 'S·Line',          "\<c-s>": 'S·Block',
    \ 'c': 'Command',          'r': 'Prompt',         't': 'Terminal',
    \ '!': 'Shell'}
  " curwin is always the number of the currently active window. In a %{}
  " context, winnr() always refers to the window to which the status line
  " being drawn belongs. Since this function is invoked in a %{} context,
  " winnr() may be different from a:curwin. We use this fact to detect
  " whether we are drawing in the active window or in an inactive window.
  function! dotfiles#statusline#setup(curwin) abort
    return get(extend(w:, { 'lf_active': winnr() ==# a:curwin  }), '', '')
  endfunction
  function! dotfiles#statusline#separator(dir, hard) abort
    if a:dir ==# 'left'
      return a:hard ? g:statusline_left_sep : g:statusline_left_soft_sep
    endif
    return a:hard ? g:statusline_right_sep : g:statusline_right_soft_sep
  endfunction
  function! dotfiles#statusline#wordcount() abort
    return &filetype =~# '\v^(markdown|txt|vimwiki)' ? wordcount().words . ' words ' : ''
  endfunction
  function! dotfiles#statusline#qfbuftype() abort
    return  (getwininfo(win_getid())[0].quickfix) ? 'Quickfix List' : 'Location List'
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
  function! dotfiles#statusline#mode_color(curwin,...) abort
    let l:inverse = get(a:, 001, 0)
    return get(g:lf_stlh, mode()) . (l:inverse ? 'Inv' : '')
  endfunction
  function! dotfiles#statusline#statusline() abort
    return '%{dotfiles#statusline#setup('.winnr().')}%#'.dotfiles#statusline#mode_color(winnr()).'#
          \'."%{w:['lf_active'] ? Componetize('dotfiles#statusline#mode()', '  ', ' ') : ''}
          \".'%{dotfiles#statusline#setup('.winnr().')}%#'.dotfiles#statusline#mode_color(winnr(), 1)."#
          \%{w:['lf_active'] && &filetype !=# 'qf' ? dotfiles#statusline#separator('left', 1) : ''}
          \%1* %f%m%h%w%r%=
          \ %y
          \%5*
          \%{Componetize('dotfiles#statusline#wordcount()', ' ', dotfiles#statusline#separator('right',0))}
          \ %l/%L:%c 
          \%2*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#warnings()') : ''}
          \%3*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#errors()', '  ') : ''}
          \%4*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#ok()', '', '  ') : ''}
          \%2*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#checking()', ' ', ' ') : ''}
          \%*"
  endfunction
" }}}
