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
      if has('nvim-0.5')
        call dotfiles#autocomplete#nvim_lsp#init()
      else
        call dotfiles#autocomplete#LanguageClient#init()
      endif
      call dotfiles#autocomplete#ncm2#init()
    endif
    " Writing:
    if g:complete_package =~# 'coc.nvim'
      call dotfiles#autocomplete#coc_nvim#writing()
    else
      call dotfiles#autocomplete#ncm2#writing()
    endif
  endif
endfunction
function! dotfiles#rg_args(...) abort
  let list = ['-S', '--smart-case', '-i', '--ignore-case', '-w', '--word-regexp',
  \ '-e', '--regexp', '-u', '--unrestricted', '-t', '--type']
  return join(list, "\n")
endfunction
