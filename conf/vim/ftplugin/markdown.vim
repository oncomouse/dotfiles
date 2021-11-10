setlocal wrap linebreak nolist spell
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
nnoremap <buffer> <silent> <leader>cc :call ToggleConcealLevel()<CR>

compiler markdown_combo

setlocal iskeyword+=',-,@-@

" Pandoc <format> to compile documents quickly and easily:
command! -nargs=1 Pandoc !pandoc -i % -o %:r.<args>
