let g:autorepl_enable = len(keys(get(g:, 'autorepl_commands', {})))
augroup autorepl
  autocmd! FileType * if g:autorepl_enable | call autorepl#start(expand('<amatch>')) | endif
augroup END
