" Theme:
set background=dark
" Only use termguicolors if in desktop mode:
if dotfiles#desktop_test()
  if exists('+termguicolors')
    let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
    " nvim-colorizer.lua:
    lua require'colorizer'.setup()
  endif
endif
" colorscheme OceanicNext
colorscheme tempus_classic
" Tempus Overrides:
hi SpellBad gui=underline guifg=#aeadaf guibg=#312e30 cterm=underline ctermfg=15
hi Pmenu gui=none guibg=#312e30 guifg=#aeadaf cterm=none ctermbg=8 ctermfg=15
hi link htmlItalic MarkdownItalic
hi link htmlBold MarkdownBold
hi link htmlTag Noise
hi link htmlEndTag Noise
let g:oceanic_next_terminal_italic = 1
let g:oceanic_next_terminal_bold = 1
function! SynGroup()                                                            
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
" # vim:foldmethod=marker
