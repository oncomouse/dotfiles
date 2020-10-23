let g:autorepl_enable = get(g:, 'autorepl_enable', 1)
augroup autorepl
  autocmd! FileType * if g:autorepl_enable | call autorepl#start(expand('<amatch>')) | endif
augroup END
