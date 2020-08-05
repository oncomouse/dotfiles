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
if &termguicolors == 1
  " Use the fish function kitty-color to change this when you update the
  " terminal:
  let s:kitty_colors = ['#232323','#d2813d','#8c9e3d','#b1942b','#6e9cb0','#b58d88','#6da280','#949d9f','#312e30','#d0913d','#96a42d','#a8a030','#8e9cc0','#d58888','#7aa880','#aeadaf'] 
  let s:cterms = map(s:kitty_colors, {i,color -> [i, color]})
else
  let s:cterms = map(range(0,15), {i -> [i]})
endif
" Terminal Colors {{{
if has('nvim') && &termguicolors == 1
  let g:terminal_color_0=s:cterms[00][1]
  let g:terminal_color_1=s:cterms[01][1]
  let g:terminal_color_2=s:cterms[02][1]
  let g:terminal_color_3=s:cterms[03][1]
  let g:terminal_color_4=s:cterms[04][1]
  let g:terminal_color_5=s:cterms[05][1]
  let g:terminal_color_6=s:cterms[06][1]
  let g:terminal_color_7=s:cterms[07][1]
  let g:terminal_color_8=s:cterms[00][1]
  let g:terminal_color_9=s:cterms[01][1]
  let g:terminal_color_10=s:cterms[02][1]
  let g:terminal_color_11=s:cterms[03][1]
  let g:terminal_color_12=s:cterms[04][1]
  let g:terminal_color_13=s:cterms[05][1]
  let g:terminal_color_14=s:cterms[06][1]
  let g:terminal_color_15=s:cterms[07][1]
  let g:terminal_color_16=s:cterms[09][1]
  let g:terminal_color_17=s:cterms[14][1]
  let g:terminal_color_foreground=s:cterms[07][1]
  let g:terminal_color_background=s:cterms[00][1]
elseif exists('*term_setansicolors') && &termguicolors == 1
  let g:terminal_ansi_colors = repeat([0], 16)
  let g:terminal_ansi_colors[0]=s:cterms[00][1]
  let g:terminal_ansi_colors[1]=s:cterms[01][1]
  let g:terminal_ansi_colors[2]=s:cterms[02][1]
  let g:terminal_ansi_colors[3]=s:cterms[03][1]
  let g:terminal_ansi_colors[4]=s:cterms[04][1]
  let g:terminal_ansi_colors[5]=s:cterms[05][1]
  let g:terminal_ansi_colors[6]=s:cterms[06][1]
  let g:terminal_ansi_colors[7]=s:cterms[07][1]
  let g:terminal_ansi_colors[8]=s:cterms[00][1]
  let g:terminal_ansi_colors[9]=s:cterms[09][1]
  let g:terminal_ansi_colors[10]=s:cterms[10][1]
  let g:terminal_ansi_colors[11]=s:cterms[11][1]
  let g:terminal_ansi_colors[12]=s:cterms[12][1]
  let g:terminal_ansi_colors[13]=s:cterms[13][1]
  let g:terminal_ansi_colors[14]=s:cterms[14][1]
  let g:terminal_ansi_colors[15]=s:cterms[15][1]
endif
" }}}
" Color Function {{{
function! <sid>hi(group, fg, bg, attr)
  " fg, bg, attr, attrsp
  if !empty(a:fg)
    exec 'hi ' . a:group . ' ctermfg=' . a:fg[0]
    if &termguicolors == 1
      exec 'hi ' . a:group . ' guifg=' .  a:fg[1]
    endif
  endif
  if !empty(a:bg)
    exec 'hi ' . a:group . ' ctermbg=' . a:bg[0]
    if &termguicolors == 1
      exec 'hi ' . a:group . ' guibg=' .  a:bg[1]
    endif
  endif
  if a:attr !=# ''
    exec 'hi ' . a:group . ' cterm=' . a:attr
    if &termguicolors == 1
      exec 'hi ' . a:group . ' gui=' .   a:attr
    endif
  endif
endfunction
" }}}
let s:ctermNONE = ['NONE', 'NONE']
hi HighlightedyankRegion ctermbg=9 guibg=#d0913d ctermfg=0 guifg=#232323
call <sid>hi('VisualMode',s:cterms[8],s:cterms[09],'bold')
call <sid>hi('InsertMode',s:cterms[8],s:cterms[02],'bold')
call <sid>hi('ReplaceMode',s:cterms[8],s:cterms[13],'bold')
call <sid>hi('CommandMode',s:cterms[8],s:cterms[6],'bold')
call <sid>hi('NormalMode',s:cterms[8],s:cterms[04],'bold')
call <sid>hi('StatusLine',s:cterms[8],s:ctermNONE,'NONE')
call <sid>hi('StatusLineNC', s:cterms[11],s:cterms[8], 'NONE')
call <sid>hi('StatusLineTerm',s:cterms[10],s:cterms[02],'NONE')
call <sid>hi('StatusLineTermNC', s:cterms[11],s:cterms[8], 'NONE')
call <sid>hi('User1', '', s:cterms[08], 'bold')
call <sid>hi('User2', '', s:cterms[08], 'NONE')
call <sid>hi('User3', s:cterms[8], s:cterms[8], 'NONE')
call <sid>hi('User4', s:cterms[8], s:cterms[01], 'NONE')
call <sid>hi('User5', s:cterms[15], s:cterms[08], 'none')
call <sid>hi('User6', s:cterms[08], s:cterms[00], 'NONE')
hi SpellBad gui=underline guifg=#aeadaf guibg=#312e30 cterm=underline ctermfg=15
hi Pmenu gui=none guibg=#312e30 guifg=#aeadaf cterm=none ctermbg=8 ctermfg=15
hi link htmlItalic MarkdownItalic
hi link htmlBold MarkdownBold
hi link htmlTag Noise
hi link htmlEndTag Noise
function! SynGroup()                                                            
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
" # vim:foldmethod=marker
