function! dotfiles#coc#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
function! dotfiles#coc#rg_args(...) abort
  let list = ['-S', '--smart-case', '-i', '--ignore-case', '-w', '--word-regexp',
  \ '-e', '--regexp', '-u', '--unrestricted', '-t', '--type']
  return join(list, "\n")
endfunction
