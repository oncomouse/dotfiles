let b:ale_fixers = []
setlocal wrap linebreak nolist spell
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
nnoremap <buffer> <silent> <leader>cc :call ToggleConcealLevel()<CR>
" Compile markdown to .docx with pandoc:
let &l:makeprg='pandoc -f markdown+smart -t docx -i % -o %:r.docx'
