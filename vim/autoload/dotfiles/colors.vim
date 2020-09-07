" Reveal the syngroup under the cursor:
function! dotfiles#colors#syn_group() abort
  let l:s = synID(line('.'), col('.'), 1)                                       
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfun
function! dotfiles#colors#termcolors() abort
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
endfunction
