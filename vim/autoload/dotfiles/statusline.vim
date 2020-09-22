" Statusline:
scriptencoding utf-8
let s:nerdfonts = g:dotfiles_mode ==# 'desktop'
" Linter Status {{{
  let g:dotfiles#ale#indicator_checking = s:nerdfonts ? "\uf110" : '…'
  let g:dotfiles#ale#indicator_warnings = s:nerdfonts ? "\uf071\u2003" : 'W: '
  let g:dotfiles#ale#indicator_errors = s:nerdfonts ? "\uf05e\u2003" : 'E: '
  let g:dotfiles#ale#indicator_information = s:nerdfonts ? "\uf7fc\u2003" : 'I: '
  let g:dotfiles#ale#indicator_ok = s:nerdfonts ? "\uf00c" : 'Ok'
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
  function! SetupStl(curwin) abort
    return get(extend(w:, { 'lf_active': winnr() ==# a:curwin  }), '', '')
  endfunction
  function! Componetize(content, ...) abort
    if strlen(a:content) == 0
      return ''
    endif
    let before = get(a:, 1, '')
    let after = get(a:, 2, '')
    return before . a:content . after
  endfunction
  function! dotfiles#statusline#statusline() abort
    return '%{SetupStl('.winnr().')}%#'.get(g:lf_stlh, mode(), 'Warnings').'#
          \ %t%m%h%w%r%q 
          \%1*%=
          \ %y 
          \'.'%{SetupStl('.winnr().')}%#'.get(g:lf_stlh, mode(), 'Warnings')."#
          \ %l/%L:%c 
          \%3*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#warnings()') : ''}
          \%4*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#errors()', '  ') : ''}
          \%5*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#ok()', '', '  ') : ''}
          \%5*%{(g:dotfiles_mode ==# 'desktop' && w:['lf_active']) ? Componetize('dotfiles#ale#checking()', '', '  ') : ''}
          \%*"
  endfunction
" }}}
" # vim:foldmethod=marker|
