setlocal wrap linebreak nolist spell
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
nnoremap <buffer> <silent> <leader>cc :call ToggleConcealLevel()<CR>

compiler markdown_combo

setlocal iskeyword+=',-

let b:surround_{char2nr('b')} = '**\r**'
let b:surround_{char2nr('l')} = '[\r](\1url: \1)'

set omnifunc=bibrefomni#omnifunc
