function! dotfiles#lsp_test() abort
  return index(keys(g:LanguageClient_serverCommands), &filetype) >=0
endfunction
