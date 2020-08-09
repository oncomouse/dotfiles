" Turns out you can just do this in Coc:
augroup lsp_ale_bridge
  autocmd!
  if &runtimepath =~# 'vim-lsp'
    autocmd User ALEWantResults call lsp_ale_bridge#getDiagnostics(g:ale_want_results_buffer)
  endif
augroup END
