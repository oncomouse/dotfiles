" Writing:
" Configure autocompletion {{{
if g:complete_package =~# 'coc.nvim'
  call dotfiles#autocomplete#coc_nvim#writing()
else
  call dotfiles#autocomplete#ncm2#writing()
endif
"
" }}}
" markdown autocmd {{{
augroup markdown-config
  autocmd!
  " Mark things bold using vim-surround (which is annoying with vim-surround):
  autocmd FileType markdown nmap <C-b> saiw*lsaiw*
  autocmd FileType markdown imap <C-b> <C-o><C-b>
  " " Set-up ncm2-biblatex:
  " autocmd FileType markdown if ! g:complete_package =~# "coc.nvim" | call dotfiles#autocomplete#ncm2#writing() | endif
  " More writing-friendly linebreaks and spelling:
  autocmd FileType markdown,text setlocal wrap linebreak nolist spell
  autocmd FileType markdown,text call textobj#sentence#init()
augroup END
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel()
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
" <leader>cc turns conceal on and off
augroup markdown_higlight
  autocmd!
  autocmd FileType markdown nnoremap <silent> <leader>cc :call ToggleConcealLevel()<CR>
augroup END
" }}}
" # vim:foldmethod=marker
