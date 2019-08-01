function! dotfiles#ag#SearchWordWithAg() abort
  execute 'Ag' expand('<cword>')
endfunction

function! dotfiles#ag#SearchVisualSelectionWithAg() range abort
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  let old_clipboard = &clipboard
  set clipboard&
  normal! ""gvy
  let selection = getreg('"')
  call setreg('"', old_reg, old_regtype)
  let &clipboard = old_clipboard
  execute 'Ag' selection
endfunction

