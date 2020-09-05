scriptencoding utf-8
set completeopt-=preview
if g:complete_package !=# 'coc.nvim'
  call dotfiles#autocomplete#ale#init()
endif
call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
if g:complete_package =~# 'coc.nvim'
else
  call dotfiles#autocomplete#ncm2#init()
  call dotfiles#autocomplete#LanguageClient#init()
endif
" # vim:foldmethod=marker
