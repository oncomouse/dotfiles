" Statusline:
scriptencoding utf-8
let g:nerdfonts = g:dotfiles_mode ==# 'desktop'
let g:statusline_soft_sep = get(g:, 'statusline_soft_sep', '⋮')
" Linter Status {{{
  let g:dotfiles#ale#indicator_checking = g:nerdfonts ? "\uf110" : '…'
  let g:dotfiles#ale#indicator_warnings = g:nerdfonts ? "\uf071\u2003" : 'W: '
  let g:dotfiles#ale#indicator_errors = g:nerdfonts ? "\uf05e\u2003" : 'E: '
  let g:dotfiles#ale#indicator_information = g:nerdfonts ? "\uf7fc\u2003" : 'I: '
  let g:dotfiles#ale#indicator_ok = ''
" }}}
" Statusline {{{
  let g:dotfiles_sl_left = g:nerdfonts ? '' : '█'
  let g:dotfiles_sl_right =g:nerdfonts ? '' : '█'
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
    " Cache ALE Errors:
    let l:ale = dotfiles#ale#aleDiagnostics()
    call extend(w:, {'ale': {
          \'errors': l:ale.error + l:ale.style_error,
          \'warnings': l:ale.total - (l:ale.error + l:ale.style_error)
          \}})
    return get(extend(w:, { 'lf_active': winnr() ==# a:curwin  }), '', '')
  endfunction
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
    return '%{dotfiles#statusline#setup('.winnr().')}
          \%2* 
          \%#'.get(g:lf_stlh, mode())."Inv#
          \%{w:['lf_active'] && dotfiles#statusline#mode() != '' ? g:dotfiles_sl_left : ''}%#".get(g:lf_stlh, mode())."#
          \%{w:['lf_active'] ? dotfiles#statusline#mode() : ''}
          \%#".get(g:lf_stlh, mode())."Inv#%{w:['lf_active'] && dotfiles#statusline#mode() != '' ? g:dotfiles_sl_right.' ' : ''}
          \%#StatusInfoInv#%{w:['lf_active'] ? (&filetype == 'qf' ? '' : ' ').g:dotfiles_sl_left : ''}
          \%1*%{w:['lf_active'] ? '' : ' '}%f%m%h%w%r%{w:['lf_active'] ? '' : ' '}
          \%#StatusInfoInv#%{w:['lf_active'] ? g:dotfiles_sl_right.' ' : ''}
          \%2*%=
          \%#StatusInfoInv#%{w:['lf_active'] && &filetype != '' ? ' '.g:dotfiles_sl_left : ''}
          \%1*%y
          \%#StatusInfoInv#%{w:['lf_active'] && &filetype != '' ? g:dotfiles_sl_right.' ' : ''}
          \%#StatusLineInfoInv#%{w:['lf_active'] ? ' '.g:dotfiles_sl_left : ''}
          \%#StatusLineInfo#
          \%{w:['lf_active'] ? '' : ' '}%l/%L:%c%{w:['lf_active'] ? '' : ' '}
          \%#StatusLineInfoInv#%{w:['lf_active'] ? g:dotfiles_sl_right : ''}
          \%#StatusWarningInv#%{w:ale.warnings != 0 && w:['lf_active'] ? '  '.g:dotfiles_sl_left : ''}
          \%#StatusWarning#%{(w:['lf_active']) ? dotfiles#statusline#componetize('dotfiles#ale#warnings()', '', '') : ''}
          \%#StatusWarningInv#%{w:ale.errors == 0 && w:ale.warnings != 0 && w:['lf_active'] ? g:dotfiles_sl_right : ''}
          \%#StatusErrorInv#%{w:ale.errors != 0 && w:ale.warnings == 0 && w:['lf_active'] ? '  '.g:dotfiles_sl_left : ''}
          \%#StatusError#%{(w:['lf_active']) ? dotfiles#statusline#componetize('dotfiles#ale#errors()', '  ', '') : ''}
          \%#StatusErrorInv#%{w:ale.errors != 0 && w:['lf_active'] ? g:dotfiles_sl_right : ''}
          \%#StatusWarningInv#%{w:['lf_active'] && dotfiles#ale#checking() != '' ? '  '.g:dotfiles_sl_left : ''}
          \%#StatusWarning#%{(w:['lf_active']) ? dotfiles#statusline#componetize('dotfiles#ale#checking()', ' ', ' ') : ''}
          \%#StatusWarningInv#%{w:['lf_active'] && dotfiles#ale#checking() != '' ? ' '.g:dotfiles_sl_right : ''}
          \%2* 
          \%*"
  endfunction
" }}}

