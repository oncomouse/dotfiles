" Turns out you can just do this in Coc:
" augroup coc-ale-bridge
"   autocmd!
"   if &runtimepath =~# 'coc'
"     autocmd User ALEWantResults call cocalebridge#getDiagnostics(g:ale_want_results_buffer)
"   endif
" augroup END
