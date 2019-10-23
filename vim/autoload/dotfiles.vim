function! dotfiles#lsp_test() abort
  return v:true
endfunction
function! dotfiles#desktop_test() abort
  return &runtimepath =~# 'coc'
endfunction
function! dotfiles#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
" Load Emmet if possible:
function! dotfiles#smart_tab() abort
  let l:emmetTypes = ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim']
  if index(l:emmetTypes, &filetype) >= 0
    return emmet#expandAbbrIntelligent("\<tab>")
  else
    return "\<tab>"
  endif
endfunction

