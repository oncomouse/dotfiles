scriptencoding utf-8
set completeopt-=preview
if g:complete_package !=# 'coc.nvim'
  call dotfiles#autocomplete#ale#init()
endif
call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
if g:complete_package =~# 'coc.nvim'
else
  call dotfiles#autocomplete#deoplete#init()
  call dotfiles#autocomplete#LanguageClient#init()
endif
nmap <leader>s :Symbols<CR>
" # vim:foldmethod=marker
