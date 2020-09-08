function! dotfiles#autocomplete#ale#init() abort
  " Check if we turned off ALE formatting in favor of LSP formatting:
  command! Format exe ':ALEFix'
  " Jump between ALE Errors:
  nmap <silent> [d :<C-u>ALEPreviousWrap<CR>
  nmap <silent> ]d :<C-u>ALENextWrap<CR>
  "
  call s:linter_options()
endfunction
function! s:linter_options() abort
  " Standard options
  " Luacheck options:

endfunction
