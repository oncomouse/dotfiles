function! dotfiles#autocomplete#ale#init() abort
  command! Format exe ':ALEFix'
  command! NextDiagnostic exe ':ALENextWrap'
  command! PreviousDiagnostic exe ':ALEPreviousWrap'
endfunction
