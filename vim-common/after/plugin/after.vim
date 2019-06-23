"
" Different highlighting for long lines:
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#182933

" Spelling colors:
hi clear SpellBad
hi! SpellBad gui=undercurl guibg=#343D46
" hi clear SpellCap
" hi clear SpellRare
" hi! SpellCap gui=undercurl guibg=#C594C5
" hi! SpellRare gui=undercurl guibg=#6699CC
"
" Buffer switching commands:
nnoremap <silent> ]b :bn<CR>
nnoremap <silent> [b :bp<CR>

