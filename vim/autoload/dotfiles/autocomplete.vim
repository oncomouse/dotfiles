" Initialize Autocomplete:
function! dotfiles#autocomplete#init() abort
  set completeopt-=preview
  if g:complete_package ==# 'coc.nvim'
    call dotfiles#autocomplete#coc_nvim#init()
  else
    " Language Server Client:
    " if has('nvim-0.5')
    "   call dotfiles#autocomplete#nvim_lsp#init()
    " else
    call dotfiles#autocomplete#LanguageClient#init()
    " endif
    " Completion:
    call dotfiles#autocomplete#ncm2#init()
    " Linter:
    call dotfiles#autocomplete#ale#init()
    " List Management:
    call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
  endif
  " Writing:
  if g:complete_package ==# 'coc.nvim'
    call dotfiles#autocomplete#coc_nvim#writing()
  else
    call dotfiles#autocomplete#ncm2#writing()
  endif
endfunction

