" Writing:
" }}}
" markdown autocmd {{{
augroup markdown-config
  autocmd!
  " More writing-friendly linebreaks and spelling:
  autocmd FileType markdown,text setlocal wrap linebreak nolist spell
  autocmd FileType markdown,text call textobj#sentence#init()
augroup END
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
" <leader>cc turns conceal on and off
augroup markdown_higlight
  autocmd!
  autocmd FileType markdown nnoremap <silent> <leader>cc :call ToggleConcealLevel()<CR>
augroup END
" }}}
" # vim:foldmethod=marker
