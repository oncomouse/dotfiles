function! dotfiles#airline#init() abort
  if airline#util#winwidth() > 79
    let g:airline_section_z = airline#section#create(['windowswap', 'obsession', 'linenr', 'maxlinenr'])
  else
    let g:airline_section_z = airline#section#create(['linenr', 'maxlinenr'])
  endif
endfunction
