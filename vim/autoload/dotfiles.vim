function! dotfiles#lsp_test() abort
  return v:true
endfunction
function! dotfiles#desktop_test() abort
  return &runtimepath =~# 'ale'
endfunction
