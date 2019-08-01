function! dotfiles#lsp_test() abort
  return index(keys(g:LanguageClient_serverCommands), &filetype) >=0
endfunction
function! dotfiles#desktop_test() abort
  return &runtimepath =~# 'ale'
endfunction
