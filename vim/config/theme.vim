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
colorscheme tempus_classic
" Terminal Colors {{{
if &termguicolors == 1
  " Use the fish function kitty-color to change this when you update the
  " terminal:
  let s:kitty_colors = ['#232323','#d2813d','#8c9e3d','#b1942b','#6e9cb0','#b58d88','#6da280','#949d9f','#312e30','#d0913d','#96a42d','#a8a030','#8e9cc0','#d58888','#7aa880','#aeadaf'] 
  let s:cterms = map(s:kitty_colors, {i,color -> [i, color]})
else
  let s:cterms = map(range(0,15), {i -> [i]})
endif
if has('nvim') && &termguicolors == 1
  let g:terminal_color_0=s:cterms[0][1]
  let g:terminal_color_1=s:cterms[1][1]
  let g:terminal_color_2=s:cterms[2][1]
  let g:terminal_color_3=s:cterms[3][1]
  let g:terminal_color_4=s:cterms[4][1]
  let g:terminal_color_5=s:cterms[5][1]
  let g:terminal_color_6=s:cterms[6][1]
  let g:terminal_color_7=s:cterms[7][1]
  let g:terminal_color_8=s:cterms[0][1]
  let g:terminal_color_9=s:cterms[1][1]
  let g:terminal_color_10=s:cterms[2][1]
  let g:terminal_color_11=s:cterms[3][1]
  let g:terminal_color_12=s:cterms[4][1]
  let g:terminal_color_13=s:cterms[5][1]
  let g:terminal_color_14=s:cterms[6][1]
  let g:terminal_color_15=s:cterms[7][1]
  let g:terminal_color_16=s:cterms[9][1]
  let g:terminal_color_17=s:cterms[14][1]
  let g:terminal_color_foreground=s:cterms[7][1]
  let g:terminal_color_background=s:cterms[0][1]
elseif exists('*term_setansicolors') && &termguicolors == 1
  let g:terminal_ansi_colors = repeat([0], 16)
  let g:terminal_ansi_colors[0]=s:cterms[0][1]
  let g:terminal_ansi_colors[1]=s:cterms[1][1]
  let g:terminal_ansi_colors[2]=s:cterms[2][1]
  let g:terminal_ansi_colors[3]=s:cterms[3][1]
  let g:terminal_ansi_colors[4]=s:cterms[4][1]
  let g:terminal_ansi_colors[5]=s:cterms[5][1]
  let g:terminal_ansi_colors[6]=s:cterms[6][1]
  let g:terminal_ansi_colors[7]=s:cterms[7][1]
  let g:terminal_ansi_colors[8]=s:cterms[0][1]
  let g:terminal_ansi_colors[9]=s:cterms[9][1]
  let g:terminal_ansi_colors[10]=s:cterms[10][1]
  let g:terminal_ansi_colors[11]=s:cterms[11][1]
  let g:terminal_ansi_colors[12]=s:cterms[12][1]
  let g:terminal_ansi_colors[13]=s:cterms[13][1]
  let g:terminal_ansi_colors[14]=s:cterms[14][1]
  let g:terminal_ansi_colors[15]=s:cterms[15][1]
endif
" }}}
" Tempus Overrides {{{
hi Visual guibg=#8e9cc0 ctermbg=12
hi Search guibg=#b1942b guifg=#232323 ctermbg=3 ctermbg=0
hi gitcommitSummary guifg=#7aa880 ctermfg=7
hi gitcommitOverflow cterm=bold gui=bold ctermbg=13 guibg=#d58888 guifg=#232323 ctermfg=0
hi HighlightedyankRegion ctermbg=9 guibg=#d0913d ctermfg=0 guifg=#232323
hi CocErrorSign guibg=#312e30 guifg=#d2813d ctermfg=1 ctermbg=8
hi CocWarningSign guibg=#312e30 guifg=#b1942b ctermfg=3 ctermbg=8
hi CocInfoSign guibg=#312e30 guifg=#d2813d ctermfg=3 ctermbg=8
hi VisualMode guifg=#312e30 ctermfg=8 guibg=#d0913d ctermbg=9 cterm=bold gui=bold
hi InsertMode guifg=#312e30 ctermfg=8 guibg=#8c9e3d ctermbg=2 cterm=bold gui=bold
hi ReplaceMode guifg=#312e30 ctermfg=8 guibg=#d58888 ctermbg=13 cterm=bold gui=bold
hi CommandMode guifg=#312e30 ctermfg=8 guibg=#C594C5 ctermbg=6 cterm=bold gui=bold
hi NormalMode guifg=#312e30 ctermfg=8 guibg=#6e9cb0 ctermbg=4 cterm=bold gui=bold
hi StatusLine guifg=#312e30 ctermfg=8 guibg=NONE ctermbg=NONE cterm=NONE gui=NONE
hi StatusLineNC guifg=#a8a030 ctermfg=11 guibg=#312e30 ctermbg=8 cterm=NONE gui=NONE
hi StatusLineTerm guifg=#96a42d ctermfg=10 guibg=#8c9e3d ctermbg=2 cterm=NONE gui=NONE
hi StatusLineTermNC guifg=#a8a030 ctermfg=11 guibg=#312e30 ctermbg=8 cterm=NONE gui=NONE
hi User1 ctermbg=8 guibg=#312e30 cterm=bold gui=bold
hi User2 ctermbg=8 guibg=#312e30 cterm=NONE gui=NONE
hi User3 guifg=#312e30 ctermfg=8 guibg=#312e30 ctermbg=8 cterm=NONE gui=NONE
hi User4 guifg=#312e30 ctermfg=8 guibg=#d2813d ctermbg=1 cterm=NONE gui=NONE
hi User5 guifg=#aeadaf ctermfg=15 guibg=#312e30 ctermbg=8 cterm=none gui=none
hi User6 guifg=#312e30 ctermfg=8 guibg=#232323 ctermbg=0 cterm=NONE gui=NONE
hi SpellBad gui=underline guifg=#aeadaf guibg=#312e30 cterm=underline ctermfg=15
hi Pmenu gui=none guibg=#312e30 guifg=#aeadaf cterm=none ctermbg=8 ctermfg=15
hi MarkdownItalic guifg=#d58888
hi link htmlItalic MarkdownItalic
hi link htmlBold MarkdownBold
hi link htmlTag Noise
hi link htmlEndTag Noise
" }}}
" Highlight Reveal Function: {{{
function! SynGroup()                                                            
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
" }}}
" # vim:foldmethod=marker
