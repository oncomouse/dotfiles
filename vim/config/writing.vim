" Writing:
" Configure autocompletion {{{
if g:complete_package =~# 'coc.nvim'
  call dotfiles#autocomplete#coc_nvim#writing()
else
  call dotfiles#autocomplete#deoplete#writing()
endif
"
" }}}
" markdown autocmd {{{
augroup markdown-config
  autocmd!
  " Mark things bold using vim-surround (which is annoying with vim-surround):
  autocmd FileType markdown nmap <C-b> ysiw*lysiw*
  autocmd FileType markdown imap <C-b> <C-o><C-b>
  " Set-up deoplete-biblatex:
  autocmd FileType markdown if ! g:complete_package =~# "coc.nvim" | call dotfiles#autocomplete#deoplete#writing() | endif
  " More writing-friendly linebreaks:
  autocmd FileType markdown set wrap linebreak nolist
  autocmd FileType markdown call textobj#sentence#init()
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
" vim-markdown {{{
let g:vim_markdown_frontmatter = 1 " Format YAML
let g:vim_markdown_strikethrough = 0 " Don't format strikethrough
let g:vim_markdown_conceal = 0 " Don't conceal
let g:vim_markdown_conceal_code_blocks = 0 " Don't conceal code blocks
let g:vim_markdown_math = 1 " Do process MathJaX and LaTeX math
" }}}
" # vim:foldmethod=marker
