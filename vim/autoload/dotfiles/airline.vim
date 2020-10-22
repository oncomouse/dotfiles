scriptencoding utf-8
function! dotfiles#airline#better_quickfix(...) abort
  if &buftype ==# 'quickfix'
    let w:airline_section_a = airline#extensions#quickfix#get_type()
    let w:airline_section_b = '%{get(w:, "quickfix_title", "")}'
    let w:airline_section_c = g:airline_powerline_fonts ? '' : 'ro'
  endif
endfunction
function! dotfiles#airline#init() abort
  if airline#util#winwidth() > 79
    let g:airline_section_z = airline#section#create(['windowswap', 'obsession', 'linenr', 'maxlinenr', ':%v'])
  else
    let g:airline_section_z = airline#section#create(['linenr', 'maxlinenr', ':%v'])
  endif
  call airline#add_statusline_func('dotfiles#airline#better_quickfix')
endfunction
