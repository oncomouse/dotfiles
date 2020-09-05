let b:ale_fixers = []
setlocal wrap linebreak nolist spell
if g:dotfiles_mode ==# 'desktop'
  call textobj#sentence#init()
endif
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
nnoremap <buffer> <silent> <leader>cc :call ToggleConcealLevel()<CR>
