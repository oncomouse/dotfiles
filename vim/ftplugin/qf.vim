if g:dotfiles_mode !=# 'desktop'
  function! QuickfixListStlName() abort
    return  (getwininfo(win_getid())[0].quickfix) ? 'Quickfix List' : 'Location List'
  endfunction
  setlocal statusline=%1*%{QuickfixListStlName()}
endif
