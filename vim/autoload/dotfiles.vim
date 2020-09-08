function! dotfiles#desktop_test() abort
  return g:dotfiles_mode ==# 'desktop'
endfunction
function! dotfiles#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
" Initialize Autocomplete:
function! dotfiles#autocomplete() abort
  if g:dotfiles_mode ==# 'desktop'
    set completeopt-=preview
    " Linter:
    if g:complete_package !=# 'coc.nvim'
      call dotfiles#autocomplete#ale#init()
    endif
    " List Management:
    call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
    " Language Server And Autocompletion:
    if !(g:complete_package =~# 'coc.nvim')
      call dotfiles#autocomplete#ncm2#init()
      call dotfiles#autocomplete#LanguageClient#init()
    endif
    " Writing:
    if g:complete_package =~# 'coc.nvim'
      call dotfiles#autocomplete#coc_nvim#writing()
    else
      call dotfiles#autocomplete#ncm2#writing()
    endif
  endif
endfunction
