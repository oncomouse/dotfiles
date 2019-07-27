" Statusline:
  let g:nerdfonts = get(g:, "nerdfonts", 1)
" Lightline Git Status {{{
  let g:lightline#gitdiff#indicator_added = '✚'
  let g:lightline#gitdiff#indicator_deleted = '✖'
  let g:lightline#gitdiff#indicator_modified = '…'
  let g:lightline#gitdiff#separator = ' ' 
" }}}
" Linter Status {{{
  let g:dotfiles#combined#indicator_checking = "\uf110"
  let g:dotfiles#combined#indicator_warnings = "\uf071\u2003"
  let g:dotfiles#combined#indicator_errors = "\uf05e\u2003"
  let g:dotfiles#combined#indicator_ok = "\uf00c"
" }}}
" Statusline {{{
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
  function! Componetize(func,...) abort
    let l:before = get(a:, 1, " ")
    let l:after = get(a:, 2, " ")
    let l:output = eval(a:func)
    if l:output == ''
      return ''
    endif
    return l:before.l:output.l:after
  endfunction
  function! BuildStatus() abort
    return '%{SetupStl('.winnr().')}%#'.get(g:lf_stlh, mode(), 'Warnings')."#
      \%{(w:['lf_active']?'  '.winnr().' ':'')}
      \%1*
      \ %{&mod?'◦':' '}%t
      \%2*%{(&paste ? g:nerdfonts ? '\uf0ea  ':' (paste) ':' ')}
      \%2*%{(w:['lf_active'] && &rtp=~'gitdiff'? Componetize('lightline#gitdiff#get()','\u22EE ') :'')}
      \%0*%=
      \%1*\ %l:%c\ 
      \%3*%{w:['lf_active'] ? Componetize('dotfiles#combined#warnings()') : ''}
      \%4*%{w:['lf_active'] ? Componetize('dotfiles#combined#errors()', '  ') : ''}
      \%5*%{w:['lf_active'] ? Componetize('dotfiles#combined#ok()', '', '  ') : ''}
      \%#".get(g:lf_stlh, mode(), 'Warnings')."#
      \%{w:['lf_active']
      \?'  '.get(g:lf_stlm,mode(),mode()).' '
      \:''}%*"
  endfunction
  set laststatus=2
  set statusline=%!BuildStatus()
" }}}
" Tabline {{{
  function! BuildTabLabel(nr, active) abort
    return (a:active ? '●' : a:nr).' '.fnamemodify(bufname(tabpagebuflist(a:nr)[tabpagewinnr(a:nr) - 1]), ":t:s/^$/[No Name]/").' '
  endfunction

  function! BuildTabLine() abort
    return (tabpagenr('$') == 1 ? '' : join(map(
      \ range(1, tabpagenr('$')),
      \ '(v:val == tabpagenr() ? "%#TabLineSel#" : "%#TabLine#") . "%".v:val."T %{BuildTabLabel(".v:val.",".(v:val == tabpagenr()).")}"'
      \ ), ''))
      \ . "%#TabLineFill#%T%=⌘ %<%{&columns < 100 ? fnamemodify(getcwd(), ':t') : getcwd()} " . (tabpagenr('$') > 1 ? "%999X✕ " : "")
  endfunction
  set tabline=%!BuildTabLine()
" }}}
" Styles {{{
  function! HighlightStatusline() abort
    hi clear StatusLine
    hi clear StatusLineNC
    hi clear TabLineFill
    hi clear TabLineSel
    hi clear TabLine
    highlight VisualMode  ctermbg=09 ctermfg=10 cterm=bold
    highlight InsertMode  ctermbg=02 ctermfg=10 cterm=bold
    highlight ReplaceMode ctermbg=01 ctermfg=10 cterm=bold
    highlight CommandMode ctermbg=05 ctermfg=10 cterm=bold
    highlight NormalMode  ctermbg=04 ctermfg=10 cterm=bold
    highlight StatusLine term=NONE cterm=bold ctermbg=NONE ctermfg=10
    highlight StatusLineNC ctermbg=NONE ctermfg=11 cterm=NONE
    highlight User1 ctermbg=8 cterm=bold
    highlight User2 ctermbg=8 cterm=NONE
    highlight User3 ctermbg=3 ctermfg=10 cterm=NONE " Linter OK
    highlight User4 ctermbg=1 ctermfg=10 cterm=NONE " Linter Error
    highlight User5 ctermbg=4 ctermfg=10 cterm=NONE " Linter Warning
    highlight TabLineFill ctermfg=11 ctermbg=NONE
    highlight TabLineSel ctermbg=11 ctermfg=13 cterm=bold
    highlight TabLine ctermbg=10 ctermfg=08
  endfunction
  augroup color-mode-switches
    autocmd!
    autocmd BufEnter * call HighlightStatusline()
    autocmd ColorScheme * call HighlightStatusline()
  augroup END
" }}}
" # vim:foldmethod=marker
