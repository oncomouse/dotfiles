" Statusline:
scriptencoding utf-8
let g:nerdfonts = g:dotfiles_mode ==# 'desktop'
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
    \ 'c': 'CommandMode', 'r': 'CommandMode',     't': 'CommandMode',
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
  function! dotfiles#statusline#statusline() abort
    return '%{SetupStl('.winnr().')}%#'.get(g:lf_stlh, mode(), 'Warnings')."#
      \%{(w:['lf_active']?'  '.winnr().' ':'')}
      \%1*
      \ %{&mod?'◦':''}%t
      \%2*%{(&paste ? g:nerdfonts ? '\uf0ea  ':' (paste) ':' ')}
      \%2*%{(w:['lf_active'] ? Componetize('gina#component#status#preset(\"fancy\")','\u22EE ') :'')}
      \%0*%=
      \%1*\ %l:%c\ 
      \%{w:['lf_active']
      \?'  '.get(g:lf_stlm,mode(),mode()).' '
      \:''}%*"
  endfunction
" }}}
" Tabline {{{
  function! BuildTabLabel(nr, active) abort
    return a:nr.' '.fnamemodify(bufname(tabpagebuflist(a:nr)[tabpagewinnr(a:nr) - 1]), ':t:s/^$/[No Name]/').TabModified(a:nr).' '
  endfunction

  function! TabModified(nr)
    let l:winnr = tabpagewinnr(a:nr)
    let l:modified = g:nerdfonts ? '✎ ' : '●'
    let l:readonly = g:nerdfonts ? ' ' : 'ro'
    return gettabwinvar(a:nr, l:winnr, '&modified') ? l:modified : gettabwinvar(a:nr, l:winnr, '&modifiable') ? '' : l:readonly
  endfunction
  function! dotfiles#statusline#tabline() abort
    return (tabpagenr('$') == 1 ? '' : join(map(
      \ range(1, tabpagenr('$')),
      \ '(v:val == tabpagenr() ? "%#TabLineSel#" : "%#TabLine#") . "%".v:val."T %{BuildTabLabel(".v:val.",".(v:val == tabpagenr()).")}"'
      \ ), ''))
      \ . "%#TabLineFill#%T%=⌘ %<%{&columns < 100 ? fnamemodify(getcwd(), ':t') : getcwd()} " . (tabpagenr('$') > 1 ? '%999X✕ ' : '')
  endfunction
" }}}
" # vim:foldmethod=marker|
