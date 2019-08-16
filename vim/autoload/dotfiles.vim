function! dotfiles#lsp_test() abort
  return v:true
endfunction
function! dotfiles#desktop_test() abort
  return &runtimepath =~# 'ale'
endfunction
function! dotfiles#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
