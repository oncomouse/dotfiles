" Initialize Autocomplete:
function! dotfiles#autocomplete#init() abort
  if g:complete_package ==# 'coc.nvim'
    " Combined Functions:
    call dotfiles#autocomplete#coc_nvim#init()
    " Writing:
    call dotfiles#autocomplete#coc_nvim#writing()
  else
    " Language Server Client:
    " if has('nvim-0.5')
    "   call dotfiles#autocomplete#nvim_lsp#init()
    " else
    call dotfiles#autocomplete#LanguageClient#init()
    " endif
    " Linter:
    call dotfiles#autocomplete#ale#init()
    " List Management:
    call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
  endif
endfunction

