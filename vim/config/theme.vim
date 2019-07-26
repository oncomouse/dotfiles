" Theme:
set background=dark
colorscheme dim
" colorscheme OceanicNext
function! SynGroup()                                                            
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
" # vim:foldmethod=marker
