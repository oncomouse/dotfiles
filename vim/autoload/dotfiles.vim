function! dotfiles#desktop_test() abort
  return g:dotfiles_mode ==# 'desktop'
endfunction
function! dotfiles#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
function! dotfiles#rg_args(...) abort
  let list = ['-S', '--smart-case', '-i', '--ignore-case', '-w', '--word-regexp',
  \ '-e', '--regexp', '-u', '--unrestricted', '-t', '--type']
  return join(list, "\n")
endfunction
