function! dotfiles#autocomplete#ale#init() abort
  command! Format exe ':ALEFix'
  nnoremap <silent> <Plug>(dotfiles-diagnostic-next) :<C-u>ALENextWrap<CR>
  nnoremap <silent> <Plug>(dotfiles-diagnostic-previous) :<C-u>ALEPreviousWrap<CR>
endfunction
