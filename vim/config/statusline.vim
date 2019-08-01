" Statusline:
let g:nerdfonts = get(g:, "nerdfonts", 1)
" Lightline Git Status {{{
  let g:lightline#gitdiff#indicator_added = '✚'
  let g:lightline#gitdiff#indicator_deleted = '✖'
  let g:lightline#gitdiff#indicator_modified = '…'
  let g:lightline#gitdiff#separator = ' ' 
  " Patches for bad buffers:
  augroup gitdiff-no-bufread
    autocmd CmdwinEnter * let g:lightline#gitdiff#cache[bufnr('%')]={}
    autocmd FileType netrw let g:lightline#gitdiff#cache[bufnr('%')]={}
  augroup END
" }}}
" Linter Status {{{
  let g:dotfiles#ale#indicator_checking = "\uf110"
  let g:dotfiles#ale#indicator_warnings = g:nerdfonts ? "\uf071\u2003" : "W: "
  let g:dotfiles#ale#indicator_errors = g:nerdfonts ? "\uf05e\u2003" : "E: "
  let g:dotfiles#ale#indicator_ok = g:nerdfonts ? "\uf00c" : "Ok"
" }}}
" Statusline {{{
  function! Componetize(func,...) abort
    let l:before = get(a:, 1, " ")
    let l:after = get(a:, 2, " ")
    let l:output = eval(a:func)
    if l:output == ''
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
  function! BuildStatus() abort
    return '%{SetupStl('.winnr().')}%#'.get(g:lf_stlh, mode(), 'Warnings')."#
      \%{(w:['lf_active']?'  '.winnr().' ':'')}
      \%1*
      \ %{&mod?'◦':''}%t
      \%2*%{(&paste ? g:nerdfonts ? '\uf0ea  ':' (paste) ':' ')}
      \%2*%{(w:['lf_active'] && &rtp=~'gitdiff'? Componetize('lightline#gitdiff#get()','\u22EE ') :'')}
      \%6*%{w:['lf_active'] && g:nerdfonts ? '':''}
      \%0*%=
      \%6*%{w:['lf_active'] && g:nerdfonts ? '':''}
      \%1*\ %l:%c\ 
      \%3*%{w:['lf_active'] ? Componetize('dotfiles#ale#warnings()') : ''}
      \%4*%{w:['lf_active'] ? Componetize('dotfiles#ale#errors()', '  ') : ''}
      \%5*%{w:['lf_active'] ? Componetize('dotfiles#ale#ok()', '', '  ') : ''}
      \%5*%{w:['lf_active'] ? Componetize('dotfiles#ale#checking()', '', '  ') : ''}
      \%#".get(g:lf_stlh, mode(), 'Warnings')."#
      \%{w:['lf_active']
      \?'  '.get(g:lf_stlm,mode(),mode()).' '
      \:''}%*"
  endfunction
" }}}
" Tabline {{{
  function! BuildTabLabel(nr, active) abort
    return a:nr.' '.fnamemodify(bufname(tabpagebuflist(a:nr)[tabpagewinnr(a:nr) - 1]), ":t:s/^$/[No Name]/").TabModified(a:nr).' '
  endfunction

  function! TabModified(nr)
    let l:winnr = tabpagewinnr(a:nr)
    let l:modified = g:nerdfonts ? '✎ ' : '●'
    let l:readonly = g:nerdfonts ? ' ' : 'ro'
    return gettabwinvar(a:nr, l:winnr, '&modified') ? l:modified : gettabwinvar(a:nr, l:winnr, '&modifiable') ? '' : l:readonly
  endfunction
  function! BuildTabLine() abort
    return (tabpagenr('$') == 1 ? '' : join(map(
      \ range(1, tabpagenr('$')),
      \ '(v:val == tabpagenr() ? "%#TabLineSel#" : "%#TabLine#") . "%".v:val."T %{BuildTabLabel(".v:val.",".(v:val == tabpagenr()).")}"'
      \ ), ''))
      \ . "%#TabLineFill#%T%=⌘ %<%{&columns < 100 ? fnamemodify(getcwd(), ':t') : getcwd()} " . (tabpagenr('$') > 1 ? "%999X✕ " : "")
  endfunction
" }}}
set statusline=%!BuildStatus()
set tabline=%!BuildTabLine()
" # vim:foldmethod=marker
