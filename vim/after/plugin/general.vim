
" Different highlighting for long lines:
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#182933

" Spelling colors:
if exists('+termguicolors')
  hi clear SpellBad
  hi! SpellBad gui=undercurl guibg=#343D46
endif
