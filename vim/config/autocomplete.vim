scriptencoding utf-8
set completeopt-=preview
" ALE {{{
if g:complete_package !=# 'coc.nvim'
  call dotfiles#autocomplete#ale#init()
endif
call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
let g:deoplete#enable_at_startup = 1
if !g:complete_package =~# 'coc.nvim'
  call dotfiles#autocomplete#LanguageClient#init()
endif
" # vim:foldmethod=marker
